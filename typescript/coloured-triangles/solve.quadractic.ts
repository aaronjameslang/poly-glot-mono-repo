export function solve(input: string): string {
  const arr = input.split("");
  let length = arr.length;
  while (length > 1) {
    stepDown(arr, length - 1);
    length -= 1;
  }
  return arr[0];
}

export function stepDown(arr: string[], length: number) {
  for (let i = 0; i < length; i += 1) {
    const here = arr[i];
    const next = arr[i + 1];
    if (here === next) {
      continue;
    }
    if (here === "B" && next === "G") {
      arr[i] = "R";
      continue;
    }
    if (here === "B" && next === "R") {
      arr[i] = "G";
      continue;
    }
    if (here === "G" && next === "B") {
      arr[i] = "R";
      continue;
    }
    if (here === "G" && next === "R") {
      arr[i] = "B";
      continue;
    }
    if (here === "R" && next === "B") {
      arr[i] = "G";
      continue;
    }
    if (here === "R" && next === "G") {
      arr[i] = "B";
      continue;
    }
  }
}
