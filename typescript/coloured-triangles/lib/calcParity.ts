export function calcParity(str: string) {
  return str.split("").reduce((acc: string, char: string) => {
    const ascii = acc.charCodeAt(0) ^ char.charCodeAt(0);
    return String.fromCharCode(ascii);
  });
}
