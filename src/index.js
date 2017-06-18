const Optimizer = require('./optimizer.js');
const AsyncIterator = require('./asyncIterator.js');
const Mentorable = require('./mentorable.js');

const MentorOptimizer = Optimizer(Mentorable);

const optimizer = new MentorOptimizer(() => null, () => false, breadth);
const iterate = state => optimizer.loop(1, 5 /** maxDepth */, state);
const getIterator = initState => new AsyncIterator(iterate, initState);
