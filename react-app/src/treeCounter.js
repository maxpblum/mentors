const listSum = l =>
  l.reduce((acc, x) => acc + x, 0);

const pickBest = (evaluate, l) =>
  l.reduce((best, x) =>
    evaluate(best, x) < 0 ? best : x);

const genList = (gen, count) => {
  const l = [];
  for (let i = 0; i < count; i++) {
    l.push(gen());
  }
  return l;
};

class TreeCounter {
  static empty() { return new Leaf(); }

  dec(item) {
    if (!this.has(item)) return [this, 0];
    return this._decRec(false, item);
  }
}

class Leaf extends TreeCounter {
  has(item) { return false; }

  inc(item) {
    const incremented = new Node(
      item, 1, new Leaf(), new Leaf());

    return [incremented, 1];
  }

  min() {
    const message = 'Tree must have at least one entry.';
    throw new Error(message);
  }

  _decRec(forceDelete, item) {
    throw new Error('Not found');
  } 

  size() { return 0; }
}

class Node extends TreeCounter {
  constructor(val, count, left, right) {
    super();
    if (!(left instanceof TreeCounter)) { throw new Error('Somehow we are adding a non-node.'); }
    if (!(right instanceof TreeCounter)) { throw new Error('Somehow we are adding a non-node.'); }
    this.val = val;
    this.count = count;
    this.left = left;
    this.right = right;
  }

  size() {
    return 1 + this.left.size() + this.right.size();
  }

  has(item) {
    if (item == this.val) { return true; }
    if (item < this.val) {
      return this.left.has(item);
    }
    return this.right.has(item);
  }

  _incThis() {
    const incremented = new Node(
      this.val,
      this.count + 1,
      this.left,
      this.right);
    return [incremented, 0];
  }

  _incLeft(item) {
    const [newLeft, sizeDiff] =
      this.left.inc(item);

    const incremented = new Node(
      this.val, this.count, newLeft, this.right);

    return [incremented, sizeDiff];
  }

  _incRight(item) {
    const [newRight, sizeDiff] =
      this.right.inc(item);

    const incremented = new Node(
      this.val, this.count, this.left, newRight);

    return [incremented, sizeDiff];
  }

  inc(item) {
    if (item == this.val) return this._incThis();
    if (item < this.val) return this._incLeft(item);
    return this._incRight(item);
  }

  min() {
    if (this.left instanceof Leaf)
      return [this.val, this.count];
    return this.left.min();
  }

  _decLeft(forceDelete, item) {
    const [newLeft, sizeDiff] =
      this.left._decRec(forceDelete, item);
    const decremented = new Node(
      this.val, this.count, newLeft, this.right);
    return [decremented, sizeDiff];
  }

  _decRight(forceDelete, item) {
    const [newRight, sizeDiff] =
      this.right._decRec(forceDelete, item);
    const decremented = new Node(
      this.val, this.count, this.left, newRight);
    return [decremented, sizeDiff];
  }

  _decThis(forceDelete) {
    if (this.left instanceof Leaf)
      return [this.right, -1];
    if (this.right instanceof Leaf)
      return [this.left, -1];
    const [newVal, newCount] = this.right.min();
    const [rWithoutMin, _] = this.right._decRec(
      true, newVal);
    const decremented = new Node(
      newVal, newCount, this.left, rWithoutMin);
    return [decremented, -1];
  }

  _decRec(forceDelete, item) {
    if (item < this.val)
      return this._decLeft(forceDelete, item);
    if (item > this.val)
      return this._decRight(forceDelete, item);
    return this._decThis(forceDelete);
  }
}

module.exports = TreeCounter;
