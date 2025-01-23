# QRCode Generator (by Joe Grinstead)

This is a simple and tiny (< 5kb!) QRCode generating library. It has one function:
```javascript
makeQrCode(errorCorrectionLevel, dataToEncode)
```
which outputs an svg path that you can then place into the dom. It gives a raw svg path instead of the full svg so that you can style the path however you like.

## Error Correction Levels
QRCodes come with some error-correcting bits, so that if a part of the code is misread (or torn off, if this were some kind of sticker), then the damaged part could be potentially recovered.

There are four levels:
- "L" - low, can recover from ~7% damage. This is perfectly fine for websites
- "M" - medium, can recover from ~15% damage.
- "Q" - quarter, can recover from ~25% damage.
- "H" - high, can recover from ~30% damage.

The higher the error-correction, the more bits you need to encode all the data, which can make for a busy QRCode.

## Example

```javascript
import {makeQrCode} from "@grinstead/makeqrcode";

const str = "Hello, World!";

// Returns the number of squares are on each side of the qr-code (including
// the spec-mandated empty space around it) as well as the path code
const { sideLength, path } = makeQrCode("L", str);

// you can make a custom svg, style it however you want!
document.body.innerHTML = `
  <svg width="320" height="320" viewBox="0 0 ${sideLength} ${sideLength}" xmlns="http://www.w3.org/2000/svg">
    <style>path {stroke: black;}</style>
    <path d="${path}" />
  </svg>`;
```

If you're using React, make sure to memoize the result to avoid expensive recomputation.
```javascript
function QRCode(props) {
  const { children: data, redundancy = "L" } = props;

  const qr = useMemo(() => makeQrCode(redundancy, data), [redundancy, data]);

  return (
    <svg
      width={props.pixels}
      height={props.pixels}
      viewBox={`0 0 ${qr.sideLength} ${qr.sideLength}`}
      xmlns="http://www.w3.org/2000/svg"
    >
      <style>{"path {stroke: black;}"}</style>
      <path d={qr.path} />
    </svg>
  );
}

function Example() {
  return <QRCode pixels={320}>Hello, World!</QRCode>;
}
```

## Supported Characters
QR Codes have several character dictionaries that are optimized. However, for simplicity, this library encodes everything as utf8 (which supports every character). This means that your generated QR Code may be "busier" (more pixels). In theory, it is possible to get better encoding performance for all-numbers or capitalized urls, but that is not implemented in this library. If you're interested in the library generating the most optimized character set, let me know!
