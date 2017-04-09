const TreeCounter = require('../src/treeCounter.js');
const {expect} = require('chai');

describe('TreeCounter', () => {
  it('initializes successfully', () => {
    expect(TreeCounter.empty());
  });
});
