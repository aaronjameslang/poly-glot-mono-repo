export function buildInput(length: number): string {
  const colours = ["R", "G", "B"];
  let result = "";
  for (let i = 0; i < length; i += 1) {
    result += colours[Math.floor(Math.random() * 3)];
  }
  return result;
}
