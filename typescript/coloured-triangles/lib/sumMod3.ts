export function sumMod3(str: string) {
  return str.split("").reduce((acc: number, char: string) => {
    const i = "RGB".indexOf(char);
    return (acc + i) % 3;
  }, 0);
}
