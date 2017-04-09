const TreeCounter = require('../src/treeCounter.js');
const {expect} = require('chai');

const processCounter =
  (initCounter, changes) => {
    const [updatedCounter, _] = changes.reduce(
      ([prevCounter, _], change) => change(prevCounter),
      [initCounter, null]);
    return updatedCounter;
  };

describe('TreeCounter', () => {
  it('initializes successfully', () => expect(TreeCounter.empty()));

  it('keeps track of members', () => {
    const nums = [4, 6, 1, 13];
    const withFourMembers = processCounter(
      TreeCounter.empty(),
      nums.map(num => counter => counter.inc(num)));
    nums.forEach(num => expect(withFourMembers.has(num)).to.be.true);
  });

  it('correctly states a member\'s absence', () => {
    const nums = [4, 6, 1, 13];
    const withFourMembers = processCounter(
      TreeCounter.empty(),
      nums.map(num => counter => counter.inc(num)));
    expect(withFourMembers.has(12)).to.be.false;
  });

  it('removes members', () => {
    const nums = [4, 6, 1, 13];
    const withFourMembers = processCounter(
      TreeCounter.empty(),
      nums.map(num => counter => counter.inc(num)));
    const withoutSix = processCounter(
      withFourMembers,
      [counter => counter.dec(6)]
    );
    [4, 1, 13].forEach(num => expect(withoutSix.has(num)).to.be.true);
    expect(withoutSix.has(6)).to.be.false;
  });

  it('correctly states size changes', () => {
    const changesAndExpectedDiffs = [
      {change: 'inc', num: 5, diff: 1},
      {change: 'dec', num: 4, diff: 0},
      {change: 'dec', num: 5, diff: -1},
      {change: 'dec', num: 5, diff: 0},
    ];
    changesAndExpectedDiffs.reduce(
      (counter, {change, num, diff}) => {
        const [newCounter, actualDiff] = counter[change](num);
        expect(actualDiff).to.equal(diff);
        return newCounter;
      },
      TreeCounter.empty()
    );
  });
});
