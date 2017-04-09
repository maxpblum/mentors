const TreeCounter = require('./treeCounter.js');

const listSum = l =>
  l.reduce((acc, x) => acc + x, 0);

const pickBest = (eval, l) =>
  l.reduce((best, x) =>
    eval(best, x) < 0 ? best : x);

const genList = (gen, count) => {
  const l = [];
  for (const i = 0; i < count; i++) {
    l.push(gen());
  }
  return l;
};
