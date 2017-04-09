const TreeCounter = require('../src/treeCounter.js');
const {expect} = require('chai');

describe('TreeCounter', () => {
  it('initializes successfully', () => expect(TreeCounter.empty()));

  it('keeps track of members', () => {
    const nums = [4, 6, 1, 13];
    const withFourMembers = nums.reduce(
      (counter, num) => counter.inc(num),
      TreeCounter.empty());
    nums.forEach(num => expect(withFourMembers.has(num)));
  });
});
