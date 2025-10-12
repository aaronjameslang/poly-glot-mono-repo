import { printRandomTriangle } from "./lib/printRandomTriangle";
import { solve } from "./solve.quadractic";
import { calcParity } from "./lib/calcParity";
import { sumMod3 } from "./lib/sumMod3";
import { printTriangle } from "./lib/printTriangle";
import { buildInputsAll } from "./lib/buildInputsAll";
import { buildInputsRandom } from "./lib/buildInputsRandom";

explore();

function explore() {
  // for (let i = 0; i < 10; i += 1) {
  //   printRandomTriangle(i);
  //   console.log("");
  // }

  // console.log("");
  // buildInputsRandom(3, 10)
  //   .map(buildRow)
  //   .sort()
  //   .forEach((msg) => console.log(msg));
  // buildInputsRandom(8, 10)
  //   .map(buildRow)
  //   .sort()
  //   .forEach((msg) => console.log(msg));
  // buildInputsRandom(9, 10)
  //   .map(buildRow)
  //   .sort()
  //   .forEach((msg) => console.log(msg));
  // buildInputsRandom(10, 10)
  //   .map(buildRow)
  //   .sort()
  //   .forEach((msg) => console.log(msg));
  // buildInputsAll().forEach((str) => printTriangle(str));
  // buildInputsAll()
  //   .map(buildRow)
  //   .sort()
  //   .forEach((msg) => console.log(msg));

  buildInputsRandom(4, 10).forEach((str) =>
    printTriangle(str, { withSum: true })
  );
}

function buildRow(input: string) {
  const colour = solve(input);
  const parityValue = calcParity(input);
  const parityNum = parityValue.charCodeAt(0);
  const guessValue = guess(input);
  const pass = guessValue === colour ? "PASS" : "FAIL";
  const sum = sumMod3(input);
  return `${pass} ${input}\t${parityValue} (${parityNum})\tmod3:${sum}  =>  Act: ${guessValue} Exp: ${colour}`;
}

function guess(str: string) {
  const p = calcParity(str);
  const mid = Math.floor(str.length / 2);
  return str[mid];
  // if (p === "W") return str[mid];
  // return p;
}
