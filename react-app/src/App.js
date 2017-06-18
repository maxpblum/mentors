import React, { Component } from 'react';
import './App.css';

import Optimizer from './optimizer.js';
import AsyncIterator from './asyncIterator.js';
import Mentorable from './mentorable.js';

const MentorOptimizer = Optimizer(Mentorable);

const optimizer = new MentorOptimizer(() => null, () => false, 5 /** breadth */);
const iterate = state => optimizer.loop(1, 5 /** maxDepth */, state);
const getIterator = (initState, onUpdate) =>
  new AsyncIterator(
    iterate,
    MentorOptimizer.initialStateFromInput(initState),
    onUpdate
  );

const convertInputToPrefs = input =>
  input
  .split('\n')
  .map(line => line.split('\t'))
  .map(lineParts => {
    const name = lineParts.slice(4).join(' ');
    const prefs = lineParts
      .slice(0, 4)
      .map(numString => Number(numString))
      .filter(num => num);

    const validName =
      Array.prototype.filter.call(name, char => char !== ' ').length > 0;

    if (prefs.length === 0 || !validName) {
      throw new Error(
        'All mentees must have a name and at least one valid pref.' +
        ` Invalid mentee with ${validName ? `name: ${name}` : 'an invalid name'}` +
        ` and ${prefs.length > 0 ? `prefs: ${prefs.join(' ')}` : 'no preferences'}.`
      );
    }

    return {name, prefs};
  });

const Choice = ({data}) => (
  <p>{`${data.name} has mentor #${data.mentor}.`}</p>
);

const Output = ({output}, index) => {
  console.log('OUTPUT: ', output);
  return (
    <div className="output" key={index}>
      {output.choices.map((choiceObj, index) =>
        <Choice data={choiceObj} key={index} />)
      }
    </div>
  );
};

class App extends Component {
  constructor() {
    super();
    this.state = {};
  }

  render() {
    return (
      <div className="App">
        <div className="App-header">
          <h2>Mentor Pairings</h2>
        </div>
        <form className="prefs" onSubmit={(e) => {
          e.preventDefault();

          if (this.state.iterator) {
            this.state.iterator.start();
          } else {
            try {
              const prefs = convertInputToPrefs(this.state.input);

              const iterator = getIterator(
                prefs,
                state =>
                  this.setState({output: state})
              );
              iterator.start();

              this.setState({iterator, error: null});
            } catch (e) {
              this.setState({error: e.message});
            }
          }
        }}>
          <label className="prefs-instructions" htmlFor="prefs">
            Enter mentor preferences below.
          </label>
          <textarea
            name="prefs"
            onChange={(e) => this.setState({input: e.target.value})}
            value={this.state.input}
          />
          <input type="submit" value="Start"/>
          <button onClick={(e) => {
            e.preventDefault();
            if (this.state.iterator)
              this.state.iterator.stop();
          }}>
            Pause
          </button>
        </form>
        <div className="output">
        {
          this.state.output ?
            <Output output={this.state.output} /> :
            null
        }
        </div>
        <div className="error">
          {this.state.error}
        </div>
      </div>
    );
  }
}

export default App;
