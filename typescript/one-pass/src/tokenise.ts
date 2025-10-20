import { FIXED_TOKENS } from "./FIXED_TOKENS";

export function* tokenise(input: string) {
  let index0 = 0;
  let index1 = 0;
  while (index1 < input.length) {
    if (startsWithWhiteSpace(input, index1)) {
      const freeToken = input.slice(index0, index1);
      if (freeToken) yield freeToken;
      index1 += 1;
      index0 = index1;
      continue;
    }
    const fixedToken = startsWithFixedToken(input, index1);
    if (fixedToken) {
      const freeToken = input.slice(index0, index1);
      if (freeToken) yield freeToken;
      index1 += fixedToken.length;
      index0 = index1;
      yield fixedToken;
      continue;
    }
    index1 += 1;
  }
  const freeToken = input.slice(index0, index1);
  if (freeToken) yield freeToken;
}

function startsWithWhiteSpace(str: string, position: number) {
  return [" ", "\n", "\t"].includes(str[position]);
}

function startsWithFixedToken(
  str: string,
  position: number,
): string | undefined {
  for (const token of FIXED_TOKENS) {
    if (str.startsWith(token, position)) {
      return token;
    }
  }
  return undefined;
}
