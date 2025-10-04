/**
 * Works from the end of the array backwards,
 *  I don't know why this is important,
 *  but it seems to meet the output format specified
 *
 * @param {T[]} arr
 * @param {T} a
 * @param {T} b
 * @returns
 */
exports.indexOfEither = function indexOfEither(arr, a, b) {
  arr = [...arr].reverse();
  const i = arr.indexOf(a);
  const j = arr.indexOf(b);
  const l = arr.length;
  const reverse = (k) => l - k - 1;

  if (i === -1 && j === -1) return -1;
  if (i === -1) return reverse(j);
  if (j === -1) return reverse(i);
  return Math.max(reverse(i), reverse(j));
};
