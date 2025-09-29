export function unchurchN(n) {
  const f = (n) => n + 1;
  const x = 0;
  return n(f)(x);
}

export function unchurchB(b) {
  return b(true)(false);
}
