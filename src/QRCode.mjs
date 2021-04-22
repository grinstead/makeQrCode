import {
  BitCanvas,
  makeBitCanvas,
  writeHorizontalRun,
  writeUpRun,
  writeValue,
  makePath,
} from "./BitCanvas.mjs";
import { GENERATOR_DATA, BLOCK_DATA } from "./RawQRCodeData.mjs";

const ORIENTATION_LENGTH = 7;
const ALIGNMENT_LENGTH = 5;
const TIMING_BAR = 6;
const SHOW_VERSION_THRESHOLD = 7;
const TWO_BYTE_LENGTH_THRESHOLD = 10;
const QUIET_ZONE = 4;
const MAX_QR_VERSION = 40;

const RESERVED_FLAG = 256;
// [RESERVED_WHITE, RESERVED_BLACK]
const SYSTEM_VALUES = [RESERVED_FLAG, 1023];

/**
 * The error correction levels L < M < Q < H
 * Why are their bit values not ordered by quality? It's a weird spec...
 */
const ERROR_CORRECTION_BITVALUES = "MLHQ";

/////////////////////////////////////////////////////////////////////////////
// Computing QR Code Data
/////////////////////////////////////////////////////////////////////////////

/**
 * Calculates the number of bits on each side of the qr code for the given version
 * @param {number} qrVersion - The QR Version (numbered 1 through 40)
 * @returns {number} The width/height in bits
 */
export function sideLengthOf(qrVersion) {
  return 17 + 4 * qrVersion;
}

/**
 * Calculates the number of alignment squares that there should be per side
 * @param qrVersion - The version of the QR Code (1 through 40)
 * @returns {number}
 */
function numAlignmentsPerSide(qrVersion) {
  return qrVersion < 2 ? 0 : (2 + qrVersion / 7) | 0;
}

/**
 * Calculates all the delineations along each side that the alignment targets
 * should occupy
 * @param {number} qrVersion - The version of the QR Code (1 through 40)
 * @returns {Array<number>} - The offsets along the edge
 */
function versionToAlignmentCoordinates(qrVersion) {
  let numPerSide = numAlignmentsPerSide(qrVersion);
  if (!numPerSide) return [];

  // we always start this 7 off the far edge
  let coordinate = sideLengthOf(qrVersion) - 7;

  // split the gap evenly, round up if odd, give slop to the gap between
  // the two smallest values. What a nightmare to figure out this formula
  let gap = Math.round((coordinate - TIMING_BAR) / (numPerSide - 1));
  gap += gap & 1;

  const positions = [TIMING_BAR];
  while (--numPerSide) {
    positions.push(coordinate);
    coordinate -= gap;
  }

  return positions;
}

/**
 * This calculates all the various mask functions
 * for a specific coordinate. The terms i and j are to match
 * the spec.
 * @param {number} i - The y-value
 * @param {number} j - The x-value
 * @returns {number} The 8-bit value for the masks at that coordinate, each bit represents the value for its corresponding mask
 */
function maskData(i, j) {
  const ijmod2 = (i * j) & 1;
  const ijmod3 = (i * j) % 3;

  const flipped =
    (((i + j) % 2 & 1) << 0) |
    ((i & 1) << 1) |
    ((j % 3 !== 0) << 2) |
    (((i + j) % 3 !== 0) << 3) |
    ((((i >> 1) + ((j / 3) | 0)) & 1) << 4) |
    ((ijmod2 + ijmod3 !== 0) << 5) |
    (((ijmod2 + ijmod3) & 1) << 6) |
    (((ijmod3 + ((i + j) & 1)) & 1) << 7);

  return flipped ^ 255;
}

/////////////////////////////////////////////////////////////////////////////
// Error Correction
/////////////////////////////////////////////////////////////////////////////

/**
 * Adds BCH error correction data to the given bits
 * @param data - The raw data
 * @param numDataBits - The number of bits in the raw data
 * @param errorPoly - The generator polynomial for this level of data
 * @returns {number} - The data concatenated with some error correction bits
 */
function addErrorCorrectionToData(data, numDataBits, errorPoly) {
  let remainder = data << (2 * numDataBits);
  for (let i = numDataBits; i >= 0; i--) {
    const difference = remainder ^ (errorPoly << i);
    remainder = difference < remainder ? difference : remainder;
  }

  return (data << (2 * numDataBits)) | remainder;
}

/**
 * Multiplies the bytes a la GF(2^8)
 * @param {number} rawA - a byte
 * @param {number} rawB - a byte
 * @returns {number} the bytes "multiplied" together
 */
function galoisMultiplication(rawA, rawB) {
  let product = 0;
  let a = rawA;
  let b = rawB;

  while (a && b) {
    if (b & 1) product ^= a;
    b >>= 1;
    a <<= 1;
    if (a & 0x100) a ^= 0x11d;
  }

  return product;
}

function computeErrorCorrectionBytes(
  bytes,
  byteStart,
  numBytes,
  coefficientsString
) {
  const maxRemainderIndex = coefficientsString.length / 2 - 1;
  const coefficients = [];
  const results = [];
  for (let i = 0; i <= maxRemainderIndex; i++) {
    coefficients.push(
      parseInt(coefficientsString.substring(2 * i, 2 * i + 2), 16)
    );
    results.push(0);
  }

  for (let i = 0; i < numBytes; i++) {
    const multiplier = bytes[byteStart + i] ^ results[0];
    for (let j = 0; j <= maxRemainderIndex; j++) {
      results[j] =
        (j === maxRemainderIndex ? 0 : results[j + 1]) ^
        galoisMultiplication(coefficients[j], multiplier);
    }
  }

  return results;
}

/////////////////////////////////////////////////////////////////////////////
// Writing to Canvas
/////////////////////////////////////////////////////////////////////////////

/**
 * Writes the big squares in the top-left, top-right, and bottom-left.
 * They are used for the QR-code to orientate itself.
 * @param {BitCanvas} canvas - The canvas to write to
 */
export function writeOrientationTargets(canvas) {
  // 0b1011101 concat 0b1000001 concat 0b1111111
  const bitPattern = 0b101110110000011111111;

  // this for-loop is intentionally fancy for code-golf purposes
  for (let i = 0; i < 3; i++) {
    // write the target itself
    for (let j = 0; j < ORIENTATION_LENGTH; j++) {
      const pattern = Math.min(j, ORIENTATION_LENGTH - 1 - j, 2);
      writeHorizontalRun(
        canvas,
        -ORIENTATION_LENGTH * (i & 1),
        -ORIENTATION_LENGTH * (i >> 1) + j,
        bitPattern >> (ORIENTATION_LENGTH * pattern),
        ORIENTATION_LENGTH,
        SYSTEM_VALUES
      );
    }

    // write the white-space around the target
    writeHorizontalRun(
      canvas,
      -(ORIENTATION_LENGTH + 1) * (i & 1),
      i < 2 ? ORIENTATION_LENGTH : -(ORIENTATION_LENGTH + 1),
      0,
      ORIENTATION_LENGTH + 1,
      SYSTEM_VALUES
    );
    writeUpRun(
      canvas,
      i & 1 ? -(ORIENTATION_LENGTH + 1) : ORIENTATION_LENGTH,
      i < 2 ? ORIENTATION_LENGTH - 1 : -1,
      0,
      ORIENTATION_LENGTH,
      SYSTEM_VALUES
    );
  }
}

/**
 * Writes an alignment target centered on the given position.
 * The alignment targets are those 5x5 smaller squares scattered around the qr code.
 * @param {BitCanvas} canvas - The BitCanvas to write to
 * @param {number} x - The x-value of the center of the target
 * @param {number} y - The y-value of the center of the target
 */
export function writeAlignmentTarget(canvas, x, y) {
  // 0b10101 concat 0b10001 concat 0b11111
  const bitPattern = 0b101011000111111;

  for (let j = 0; j < ALIGNMENT_LENGTH; j++) {
    const pattern = Math.min(j, ALIGNMENT_LENGTH - 1 - j);
    writeHorizontalRun(
      canvas,
      x - 2,
      y - 2 + j,
      bitPattern >> (ALIGNMENT_LENGTH * pattern),
      ALIGNMENT_LENGTH,
      SYSTEM_VALUES
    );
  }
}

/**
 * Writes the format information to the canvas, it will write each potential masks value
 * @param {BitCanvas} canvas - The canvas to write to
 * @param {number} errorCorrection - The error correction
 */
function writeFormatBits(canvas, errorCorrection) {
  const Xs = [-1, -2, -3, -4, -5, -6, -7, -8, 7, 5, 4, 3, 2, 1, 0];
  const Ys = [0, 1, 2, 3, 4, 5, 7, 8, -7, -6, -5, -4, -3, -2, -1];

  // x^10 + x^8 + x^5 + x^4 + x^2 + x + 1
  const generatorPolynomial = 0b10100110111;

  // generate one per mask
  const formats = [];
  for (let mask = 0; mask < 8; mask++) {
    const raw = addErrorCorrectionToData(
      (errorCorrection << 3) | mask,
      5,
      generatorPolynomial
    );

    // spec says to xor this in to prevent all 0s
    formats.push(raw ^ 0b101010000010010);
  }

  const bits = canvas.bits;
  for (let i = Xs.length; --i >= 0; ) {
    let value = RESERVED_FLAG;
    for (let mask = 0; mask < 8; mask++) {
      value |= ((formats[mask] >> i) & 1) << mask;
    }

    writeValue(canvas, Xs[i], 8, value);
    writeValue(canvas, 8, Ys[i], value);
  }

  // reserve that one space that was skipped
  writeValue(canvas, 8, -8, SYSTEM_VALUES[1]);
}

/**
 * Writes the version to the qr code (if necessary)
 * @param {BitCanvas} canvas
 * @param {number} qrVersion - The qr version (1 through 40)
 */
function writeVersionBits(canvas, qrVersion) {
  if (qrVersion < SHOW_VERSION_THRESHOLD) return;

  // x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1
  const generatorPolynomial = 0b1111100100101;
  const version = addErrorCorrectionToData(qrVersion, 6, generatorPolynomial);

  // write to the bottom-left
  for (let i = 5; i >= 0; i--) {
    const bits = version >> (3 * i);
    writeUpRun(canvas, i, -9, bits, 3, SYSTEM_VALUES);

    // this is the only place where things go left-to-right... why!
    // (to make it symmetrical, but still annoying)
    writeHorizontalRun(
      canvas,
      -11,
      i,
      ((bits & 1) << 2) | (bits & 2) | ((bits & 4) >> 2),
      3,
      SYSTEM_VALUES
    );
  }
}

/////////////////////////////////////////////////////////////////////////////
// Assembling QR Codes
/////////////////////////////////////////////////////////////////////////////

function makeContentWriter(bits, rawSideLength) {
  const sideLength = rawSideLength - 1; // we remove the timing bar

  let bitX = sideLength - 2;
  let bitY = sideLength;
  let goingUp = true;

  return (byte) => {
    for (let i = 7; i >= 0; i--) {
      let bitIndex = 0;

      // iterate until we find an open bit
      do {
        if (bitX & 1) {
          bitX--;
        } else if (goingUp) {
          if (bitY) {
            bitY--;
            bitX++;
          } else {
            goingUp = false;
            bitX--;
          }
        } else {
          if (++bitY < sideLength) {
            bitX++;
          } else {
            goingUp = true;
            bitY--;
            bitX--;
          }
        }

        // the spec says to treat the timing bars as not being there
        bitIndex =
          (bitY + (bitY >= TIMING_BAR)) * rawSideLength +
          (bitX + (bitX >= TIMING_BAR));
      } while (bits[bitIndex]);

      bits[bitIndex] = byte & (1 << i) ? 255 : 0;
    }
  };
}

/**
 * Creates the skeleton of the QR Code, that is all the alignment information
 * @param qrVersion - The version of the qr code (1 thru 40)
 * @returns {BitCanvas} The canvas (for now)
 */
export function makeBaseQrCode(qrVersion) {
  const side = sideLengthOf(qrVersion);
  const canvas = makeBitCanvas(side, side, 0);

  // timing lines
  for (let i = 7; i < side; i += 8) {
    writeUpRun(canvas, TIMING_BAR, i, 0b1010101, 8, SYSTEM_VALUES);
    writeHorizontalRun(canvas, i, TIMING_BAR, 0b1010101, 8, SYSTEM_VALUES);
  }

  // orientation targets
  writeOrientationTargets(canvas);

  // alignment targets
  const alignments = versionToAlignmentCoordinates(qrVersion);
  const numAlignments = alignments.length;
  for (let i = numAlignments * numAlignments; i-- > 0; ) {
    if (i > 1 && i !== numAlignments) {
      writeAlignmentTarget(
        canvas,
        alignments[i % numAlignments],
        alignments[(i / numAlignments) | 0]
      );
    }
  }

  writeVersionBits(canvas, qrVersion);

  return canvas;
}

/**
 * Runs the scoring algorithm (part of the spec).
 * This is _almost_ fully to spec (at least, I think it is).
 * I don't check for the black-white ratio at all sizes, just the pattern 1011101.
 * @param {BitCanvas} canvas - The qr code to score
 * @param {number} mask - The id of the mask
 * @param {Array<number>} rectData - Hard to explain this one... It is overwritten with dynamic programming techniques to contain rectangle data
 */
function scoreMask(canvas, mask, rectData) {
  const size = canvas.canvasWidth;
  const bits = canvas.bits;

  let score = 0;

  let numBlack = 0;

  const badPatternCost = 40;
  const badPattern = 0b1011101;

  let runLength = 0;
  function scoreRun() {
    if (runLength >= 5) {
      score += runLength;
    }
    runLength = 0;
  }

  for (let y = 0, i = 0; y < size; y++) {
    let pattern = 0;

    for (let x = 0; x < size; x++, i++) {
      const bit = (bits[i] >> mask) & 1;

      // keep track of the number of black bits
      numBlack += bit;

      const rectDataI = x * size + y;

      if (bit ^ (pattern & 1)) {
        // the bit does not match the previous bit
        scoreRun();
      }

      rectData[rectDataI] = ++runLength;

      // check if we match the bit above
      if (y) {
        if (bit === ((bits[i - size] >> mask) & 1)) {
          // the bit matches the bit above, so add up all the rectangles

          let width = runLength;
          for (
            let heightMinus1 = 1;
            width && heightMinus1 <= y;
            heightMinus1++
          ) {
            width = Math.min(width, rectData[rectDataI - heightMinus1]);

            // for next row
            rectData[rectDataI - heightMinus1] = width;

            // normally the score would be 3 * (width - 1) * (height - 1)
            // however, we have to add in the score for each sub-rectangle
            // that has this pixel at the bottom right
            const sumOfWidths = ((width - 1) * width) / 2;

            score += 3 * heightMinus1 * sumOfWidths;
          }
        } else {
          // because this pixel does not match above
          // we know it is not going to be the bottom-right
          // of any rectangle (more than 1 tall)
          rectData[rectDataI - 1] = 0;
        }
      }

      // check bad pattern
      pattern = ((pattern & 63) << 1) | bit;
      if (badPattern === pattern) {
        score += badPatternCost;
      }
    }

    scoreRun();
  }

  // Score approximately equal amounts of white/black squares
  const percentageOff = Math.abs(numBlack / (size * size) - 0.5);
  score += 10 * Math.floor(20 * percentageOff);

  // Look for black-white-black-black-black-white-black
  // which looks like the big targets
  // we only need to do vertically, because we already did
  // the horizontal checking

  for (let x = 0; x < size; x++) {
    let pattern = 0;

    for (let y = 0, i = x; y < size; y++, i += size) {
      const bit = (bits[i] >> mask) & 1;

      if (bit ^ (pattern & 1)) {
        // the bit does not match the previous bit
        scoreRun();
      }
      runLength++;

      pattern = ((pattern & 63) << 1) | bit;
      if (badPattern === pattern) {
        score += badPatternCost;
      }
    }

    scoreRun();
  }

  return score;
}

export function makeQrCode(level, string) {
  const levelBits = ERROR_CORRECTION_BITVALUES.indexOf(level);
  if (levelBits < 0) return; // error, not one of L M Q or H

  // starts with the utf8 header, which is 0111 (Extended Channel Interpretation)
  // the type 00011001 (utf8) and a footer saying to read bytes, the last two
  // numbers will represent the number of characters
  const encoded = [0b01110001, 0b10100100, 0, 0];
  for (let i = 0; i < string.length; i++) {
    let codepoint = string.charCodeAt(i);
    if ((0xfc00 & codepoint) === 0xd800) {
      // note that this implicitly returns NaN if i === string.length - 1
      // which will get "correctly" encoded as the raw value in utf8
      // basically, if we run into lone surrogate pairs, we encode
      // them into the utf8
      const nextChar = string.charCodeAt(i + 1);

      if ((0xfc00 & nextChar) === 0xdc00) {
        const rawBits = ((codepoint & 0x3ff) << 10) | (nextChar & 0x3ff);
        codepoint = rawBits + 0x10000;
      }
    }

    if (codepoint < 1 << 7) {
      encoded.push(codepoint);
    } else if (codepoint < 1 << 11) {
      encoded.push(0xc0 | (codepoint >> 6), 0x80 | (codepoint & 0x3f));
    } else if (codepoint < 1 << 16) {
      encoded.push(
        0xe0 | (codepoint >> 12),
        0x80 | ((codepoint >> 6) & 0x3f),
        0x80 | (codepoint & 0x3f)
      );
    } else {
      encoded.push(
        0xf0 | (codepoint >> 18),
        0x80 | ((codepoint >> 12) & 0x3f),
        0x80 | ((codepoint >> 6) & 0x3f),
        0x80 | (codepoint & 0x3f)
      );
    }
  }

  const contentByteCount = encoded.length - 4;
  encoded[2] = contentByteCount >> 8;
  encoded[3] = contentByteCount & 0xff;

  const options = BLOCK_DATA[levelBits];
  let qrVersion = 1;
  let blockInfo = 0;
  let numDataBytes = 0;
  do {
    blockInfo = options[qrVersion - 1];
    numDataBytes = blockInfo >> 12;
  } while (
    contentByteCount + (qrVersion < TWO_BYTE_LENGTH_THRESHOLD ? 3 : 4) >
      numDataBytes &&
    ++qrVersion <= MAX_QR_VERSION
  );

  if (qrVersion > MAX_QR_VERSION) return;

  // remove the extra length byte if it was unnecessary
  if (qrVersion < TWO_BYTE_LENGTH_THRESHOLD) {
    encoded.splice(2, 1);
  }

  // pad 0s
  while (encoded.push(0) < numDataBytes) {}

  const generator = GENERATOR_DATA.split("|")[blockInfo & 0x1f];
  const numErrorBytesPerBlock = generator.length / 2;
  const numBlocks = (blockInfo >> 5) & 0x7f;

  const blocks = [];
  const numDataBytesPerBlock = Math.floor(numDataBytes / numBlocks);
  const overflowBlockIndex = numBlocks - (numDataBytes % numBlocks);
  for (let i = 0; i < numBlocks; i++) {
    const startIndex =
      i * numDataBytesPerBlock + Math.max(0, i - overflowBlockIndex);
    blocks.push({
      startIndex,
      errorBytes: computeErrorCorrectionBytes(
        encoded,
        startIndex,
        numDataBytesPerBlock + (i >= overflowBlockIndex),
        generator
      ),
    });
  }

  const side = sideLengthOf(qrVersion);
  const canvas = makeBaseQrCode(qrVersion);

  writeFormatBits(canvas, levelBits);

  const writeByte = makeContentWriter(canvas.bits, side);

  // write the plain content
  for (let i = 0, blockIndex = 0, withinBlock = 0; i < numDataBytes; i++) {
    const block = blocks[blockIndex];
    writeByte(encoded[block.startIndex + withinBlock]);

    if (++blockIndex >= blocks.length) {
      // loop back to the first block, or the first overflow block if necessary
      blockIndex =
        ++withinBlock < numDataBytesPerBlock ? 0 : overflowBlockIndex;
    }
  }

  // write the error correction
  for (let i = 0; i < numErrorBytesPerBlock; i++) {
    for (let j = 0; j < blocks.length; j++) {
      writeByte(blocks[j].errorBytes[i]);
    }
  }

  // add the masks to the non-system squares
  const bits = canvas.bits;
  for (let i = 0; i < side; i++) {
    for (let j = 0; j < side; j++) {
      const index = i * side + j;
      const bit = bits[index];
      if (bit < RESERVED_FLAG) {
        bits[index] = bit ^ maskData(i, j);
      }
    }
  }

  let bestMask = 0;
  let bestMaskScore = 0;

  const scratchPad = canvas.bits.slice();
  for (let mask = 0; mask < 8; mask++) {
    const score = scoreMask(canvas, mask, scratchPad);
    if (!mask || score < bestMaskScore) {
      bestMask = mask;
      bestMaskScore = score;
    }
  }

  const path = makePath(canvas, QUIET_ZONE, QUIET_ZONE, 1 << bestMask);

  return { sideLength: side + 2 * QUIET_ZONE, path };
}
