import { makeQrCode as rawMakeQrCode } from "./QRCode.mjs";

window["makeQrCode"] = (level, string) => {
  const result = rawMakeQrCode(level, string);

  // prettier-ignore
  return { "sideLength": result.sideLength, "path": result.path };
};
