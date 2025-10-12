export function buildInputsAll() {
  const inputs = [];
  for (let i = 0; i < 3; i += 1) {
    for (let j = 0; j < 3; j += 1) {
      for (let k = 0; k < 3; k += 1) {
        const colours = ["R", "G", "B"];
        const row = colours[i] + colours[j] + colours[k];
        inputs.push(row);
      }
    }
  }
  return inputs;
}
