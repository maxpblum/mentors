module.exports = class AsyncIterator {
  constructor(iterate, initState, onUpdate) {
    this.iterate = iterate;
    this.state = initState;
    this.timeout = null;
    this.onUpdate = onUpdate;
  }

  start() {
    if (this.timeout === null) {
      this.scheduleIteration();
    }
  }

  stop() {
    clearTimeout(this.timeout);
    this.timeout = null;
  }

  runIteration() {
    console.log('actually in runIteration, state: ', this.state);
    this.state = this.iterate(this.state);
    console.log(this.state);
    this.onUpdate(this.state);
    this.scheduleIteration();
  }

  scheduleIteration() {
    console.log('iterating, state: ', this.state);
    this.timeout = setTimeout(this.runIteration.bind(this), 0);
  }
};
