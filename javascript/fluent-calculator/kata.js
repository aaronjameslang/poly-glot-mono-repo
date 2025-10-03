function FluentNumber(n) {
  return {
    get minus() {
      return FluentOperation((m) => n - m);
    },
    get plus() {
      return FluentOperation((m) => n + m);
    },
    get times() {
      return FluentOperation((m) => n * m);
    },
    get dividedBy() {
      return FluentOperation((m) => n / m);
    },
    valueOf: function () {
      return n;
    },
  };
}

function FluentOperation(f) {
  return {
    get zero() {
      return FluentNumber(f(0));
    },
    get one() {
      return FluentNumber(f(1));
    },
    get two() {
      return FluentNumber(f(2));
    },
    get three() {
      return FluentNumber(f(3));
    },
    get four() {
      return FluentNumber(f(4));
    },
    get five() {
      return FluentNumber(f(5));
    },
    get six() {
      return FluentNumber(f(6));
    },
    get seven() {
      return FluentNumber(f(7));
    },
    get eight() {
      return FluentNumber(f(8));
    },
    get nine() {
      return FluentNumber(f(9));
    },
    get ten() {
      return FluentNumber(f(10));
    },
  };
}

function FluentCalculator() {
  return FluentOperation((m) => m);
}

module.exports = { FluentCalculator: FluentCalculator() };
