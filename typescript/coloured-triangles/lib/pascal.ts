const FIRST_ROW = [1];

function nextRow(prev: number[]) {
  const next = new Array(prev.length + 1);
  for (let i = 0; i < next.length; i += 1) {
    next[i] = (prev[i - 1] ?? 0) + (prev[i] ?? 0);
  }
  return next;
}

function printPascalTriangle(limit: number) {
  let arr = FIRST_ROW;
  for (let i = 0; i < limit; i += 1) {
    console.log(" ".repeat(limit - i) + arr.join(" "));
    arr = nextRow(arr);
  }
}

printPascalTriangle(10);

function sumMod3(arr: number[]) {
  return arr.reduce((a, b) => a + Math.abs(b % 3), 0);
}

function findDeepestRowForEachSum(limit: number) {
  let arr = FIRST_ROW;
  const results = [];
  for (let i = 0; i < limit; i += 1) {
    const sum = sumMod3(arr);
    results[sum] = i;
    arr = nextRow(arr);
  }
  const table = results
    .map((row, sum) => ({ sum, row, score: row / sum }))
    .filter(Boolean)
    .sort((a, b) => b.score - a.score)
    .slice(0, 100);
  console.table(table);
}

// findDeepestRowForEachSum(10_000);

function findRowsWithSumLte4(limit: number) {
  let arr = FIRST_ROW;
  const results = [];
  for (let i = 0; i < limit; i += 1) {
    if (i == 81) console.log(arr);
    const sum = sumMod3(arr);
    if (sum <= 4) {
      results.push({ sum, i });
    }
    arr = nextRow(arr);
  }
  console.table(results);
}

// findRowsWithSumLte4(100);

// We're hitting max safe int and it's muddying results

function nextRowMod3(prev: number[]) {
  const next = new Array(prev.length + 1);
  for (let i = 0; i < next.length; i += 1) {
    next[i] = ((prev[i - 1] ?? 0) + (prev[i] ?? 0)) % 3;
  }
  return next;
}

function printPascalTriangleMod3(limit: number) {
  let arr = FIRST_ROW;
  for (let i = 0; i < limit; i += 1) {
    console.log(" ".repeat(limit - i) + arr.join(" "));
    arr = nextRowMod3(arr);
  }
}

printPascalTriangleMod3(10);

function findRowsWithSumLte4Mod3(limit: number) {
  let arr = FIRST_ROW;
  const results = [];
  for (let i = 0; i < limit; i += 1) {
    if (i == 81) console.log(arr);
    const sum = sumMod3(arr);
    if (sum <= 4) {
      results.push({ sum, i });
    }
    arr = nextRowMod3(arr);
  }
  console.table(results);
}

findRowsWithSumLte4Mod3(100);
