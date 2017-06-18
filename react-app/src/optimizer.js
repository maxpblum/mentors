const pickBest = (evaluate, l) =>
  l.reduce((best, x) =>
    evaluate(best, x) < 0 ? best : x);

const genList = (gen, count) => {
  const l = [];
  for (let i = 0; i < count; i++) l.push(gen());
  return l;
};

module.exports = function Optimizer(Optimizable) {
  return class Optimizer extends Optimizable {
    constructor(report, ready, breadth) {
      super();
      this.report = report;
      this.ready = ready;
      this.breadth = breadth;
    }

    tree(depth, maxDepth, state) {
      if (depth === maxDepth) return state;

      const doTry = () => this.tree(
        depth + 1, maxDepth, Optimizable.iter(state));

      const tries = genList(doTry, this.breadth);

      return pickBest(Optimizable.evaluate, tries);
    }

    loop(timesLeft, maxDepth, state) {
      console.log('state in optimizer: ', state);
      console.log(`${timesLeft} times left.`);
      this.report(state);

      if (this.ready(state) || timesLeft <= 0)
        return state;

      const newState = this.tree(
        0, maxDepth, state);

      const chosenState = pickBest(
        Optimizable.evaluate, [state, newState]);

      const newMaxDepth = (chosenState === newState) ?
        maxDepth : maxDepth + 1;

      return this.loop(timesLeft - 1, newMaxDepth, chosenState);
    }

    static optimize(
      report,
      ready,
      breadth,
      depth,
      loopTimes,
      data) {
        const state = Optimizable.initialStateFromInput(data);
        const optimizer = new Optimizer(
          report, ready, breadth)
        const optimized = optimizer.loop(
          loopTimes, depth, state);
        report(optimized);
        return optimized;
    }

    static iterate() {}
  };
};
