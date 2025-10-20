export class Peekerator<T> {
  private readonly iter: Iterator<T>;
  private _next: IteratorResult<T> | undefined;

  constructor(iter: Iterator<T> | Iterable<T>) {
    this.iter = iterator(iter);
  }

  peek(): IteratorResult<T> {
    if (!this._next) this._next = this.iter.next();
    return this._next;
  }

  next(): IteratorResult<T> {
    if (this._next) {
      const x = this._next;
      this._next = undefined;
      return x;
    }
    return this.iter.next();
  }
}

export function iterator<T>(iter: Iterator<T> | Iterable<T>): Iterator<T> {
  return isIterable(iter) ? iter[Symbol.iterator]() : iter;
}

export function isIterable<T>(iter: unknown): iter is Iterable<T> {
  if (iter == null) return false;
  if (typeof iter !== "object") return false;
  return Symbol.iterator in iter;
}
