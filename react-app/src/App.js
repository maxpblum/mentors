import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor() {
    super();
    this.state = {output: 'Initial output value.'};
  }

  render() {
    return (
      <div className="App">
        <div className="App-header">
          <h2>Mentor Pairings</h2>
        </div>
        <form className="prefs" onSubmit={(e) => {
          e.preventDefault();
          console.log(this.state.input);
          this.setState({
            output: this.state.input,
            input: ''
          });
        }}>
          <label className="prefs-instructions" htmlFor="prefs">
            Enter mentor preferences below.
          </label>
          <textarea
            name="prefs"
            onChange={(e) => this.setState({input: e.target.value})}
            value={this.state.input}
          />
          <input type="submit"/>
        </form>
        <div className="output">
          {this.state.output}
        </div>
      </div>
    );
  }
}

export default App;
