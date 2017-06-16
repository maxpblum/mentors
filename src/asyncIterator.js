module.exports = class AsyncIterator {
  constructor(iterate, initState) {
    this.iterate = iterate;
    this.state = initState;
    this.timeout = null;
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

  iterate() {
    this.state = this.iterate(this.state);
    this.scheduleIteration();
  }

  scheduleIteration() {
    this.timeout = setTimeout(this.iterate.bind(this), 0);
  }
}
