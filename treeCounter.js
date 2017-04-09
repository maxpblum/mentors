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

class TreeCounter {
  static empty() { return new Leaf(); }

  dec(item) {
    if (!this.mem(item)) return [this, 0];
    return this._decRec(false, item);
  }
}

class Leaf extends TreeCounter {
  mem(item) { return false; }

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
    this.val = val;
    this.count = count;
    this.left = left;
    this.right = right;
  }

  // TODO: Rename to isMember.
  mem(item) { 
    if (item == this.val) { return true; }
    if (item < this.val) {
      return this.left.mem(item);
    }
    return this.right.mem(item);
  }

  _incThis(item) {
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
    if (item < this.val) return this._incLeft();
    return this._incRight();
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

  _decThis(forceDelete, item) {
    if (this.left instanceof Leaf)
      return [this.right, -1];
    if (this.right instanceof Leaf)
      return [this.left, -1];
    const [newVal, newCount] = this.right.min();
    const rWithoutMin = this.right._decRec(
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
    return this._decThis(forceDelete, item);
  }
}

module.exports = TreeCounter;
