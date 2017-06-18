const Optimizer = require('../src/optimizer.js');
const {expect} = require('chai');

const count = (l, item) =>
  l.reduce(
    (acc, member) =>
      member === item ? acc + 1 : acc,
    0
  );

const shuffle = (array) => {
  let currentIndex = array.length
  let temporaryValue;
  let randomIndex;

  while (0 !== currentIndex) {

    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }
};

const shuffled = (array) => {
  const copy = array.slice();
  shuffle(copy);
  return copy;
};

describe('Optimizer', () => {
  it('constructs successfully', () => {
    class Dummy {}
    const DummyOptimizer = Optimizer(Dummy);
    expect(new DummyOptimizer()).to.exist;
  });

  it('optimizes in a tree structure', () => {
    // This is a bad test, jettison it if it starts breaking
    // but everything else seems to work.
    const outputCatcher = [];
    class Simple {
      static iter(depth) {
        const newDepth = depth + 1;
        outputCatcher.push(newDepth);
        return newDepth;
      }
      static evaluate(a, b) { return a; }
    }
    const SimpleOptimizer = Optimizer(Simple);
    const simpleOptimizer = new SimpleOptimizer(
      () => null, () => null, 3);

    simpleOptimizer.tree(1, 3, 1);

    expect(count(outputCatcher, 1)).to.equal(0);
    expect(count(outputCatcher, 2)).to.equal(3);
    expect(count(outputCatcher, 3)).to.equal(9);
  });

  it('optimizes sanely', () => {
    // Create an ever-increasing counter so that more iteration will
    // always lead to higher values. Test that of all the values
    // found, the optimizer picks the very highest.

    let i = 1;
    const getNext = () => ++i;

    class HighestIsBest {
      static iter() { return getNext(); }
      static evaluate(a, b) { return a > b ? a : b; }
      static initialStateFromInput(data) { return data; }
      static statsOfState(state) { return `State is ${state}.`; }
    }
    const output = Optimizer(HighestIsBest).optimize(() => null, () => null, 3, 4, 1, i);

    expect(output).to.equal(i);
  });
});
