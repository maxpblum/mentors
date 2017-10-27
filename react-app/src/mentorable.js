const TreeCounter = require('./treeCounter.js');
const Optimizer = require('./optimizer.js');

/**
 * @typedef {{
 *   name: string,
 *   prefs: !Array<number>,
 * }}
 */
let Request;

const sum = l => l.reduce((sum, n) => sum + n, 0);
const pickBest = (l, evaluate) => {
  if (l.length < 2) {
    throw new Error('Must pick from at least two entries.');
  }
  return l.reduce((best, n) =>
    evaluate(best, n) < 0 ? best : n);
};

const withRandomChangedEntry = (list, changer) => {
  const index = Math.floor(Math.random() * list.length);
  const changedList = list.slice();
  const oldValue = changedList[index];
  const {changeData, newValue} = changer(oldValue);
  changedList[index] = newValue;
  return {changeData, changedList};
};

const mean = list => sum(list) / list.length;
const getRank = (list, entry) => list.indexOf(entry) + 1;

class MentorPrefs {
  static getOneScore(prefs, match) {
    for (let i = 0; i < prefs.length; i++) {
      if (prefs[i] === match) return 14 - i;
    }
    throw new Error('Should only assign chosen mentors');
  }

  static rankScore(pairings) {
    const scoreNext = (sum, {name, mentor, prefs}) =>
      sum + MentorPrefs.getOneScore(prefs, mentor);
    return pairings.reduce(scoreNext, 0);
  }

  static incrementMentorFromPref(mentorCounts, {mentor}) {
    return mentorCounts.inc(mentor)[0];
  }

  static deficit(pairings) {
    const taken = pairings.reduce(
      MentorPrefs.incrementMentorFromPref,
      TreeCounter.empty()
    );
    return taken.size() - pairings.length;
  }

  static adjustedScore(pairings) {
    const deficit = MentorPrefs.deficit(pairings);
    return deficit < 0 ? deficit : MentorPrefs.rankScore(pairings);
  }

  static evaluate(pairingsA, pairingsB) {
    return MentorPrefs.adjustedScore(pairingsB.choices) - MentorPrefs.adjustedScore(pairingsA.choices);
  }

  static initializePref({name, prefs}) {
    if (prefs.length < 1) {
      throw new Error('Each mentee must have at least one preferred mentor.');
    }
    return {name, prefs, mentor: prefs[0]};
  }

  static initialStateFromInput(data) {
    const firstChoicePairings = data.map(MentorPrefs.initializePref);
    const mentorCounts = firstChoicePairings.reduce(
      MentorPrefs.incrementMentorFromPref,
      TreeCounter.empty()
    );
    const firstScore = MentorPrefs.adjustedScore(firstChoicePairings);
    return {
      score: firstScore,
      rankScore: MentorPrefs.rankScore(firstChoicePairings),
      deficit: MentorPrefs.deficit(firstChoicePairings),
      choices: firstChoicePairings,
      taken: mentorCounts,
    }
  }

  static assignmentWithoutPrefs({name, mentor}) {
    return {name, mentor};
  }

  static outputFromState({choices}) {
    return choices.map(MentorPrefs.assignmentWithoutPrefs);
  }

  static withRandomlySwitchedMentor({name, prefs, mentor}) {
    const otherOptions = prefs.filter(p => p !== mentor);
    // If there weren't multiple unique choices, default to the original.
    const availableChoices = otherOptions.length > 0 ? otherOptions : prefs;
    const choice = Math.floor(Math.random() * availableChoices.length);
    return {name, prefs, mentor: availableChoices[choice]};
  }

  static randomSwitchWithScore(requestWithAssignment) {
    const {prefs} = requestWithAssignment;
    const oldScore = MentorPrefs.getOneScore(prefs, requestWithAssignment.mentor);
    const newRequestWithAssignment =
      MentorPrefs.withRandomlySwitchedMentor(requestWithAssignment);

    const newScore = MentorPrefs.getOneScore(prefs, newRequestWithAssignment.mentor);
    return {
      changeData: {
        scoreDiff: newScore - oldScore,
        oldMentor: requestWithAssignment.mentor,
        newMentor: newRequestWithAssignment.mentor,
      },
      newValue: newRequestWithAssignment,
    };
  }

  static iter(state) {
    const {changeData: {oldMentor, newMentor, scoreDiff}, changedList} =
      withRandomChangedEntry(state.choices, MentorPrefs.randomSwitchWithScore);
    const [counterAfterDec, diffAfterDec] = state.taken.dec(oldMentor);
    const [newCounter, diffAfterInc] = counterAfterDec.inc(newMentor);
    const deficit = state.deficit + diffAfterDec + diffAfterInc;
    const newScore = MentorPrefs.adjustedScore(changedList);
    return {
      choices: changedList,
      score: newScore,
      rankScore: MentorPrefs.rankScore(changedList),
      deficit,
      taken: newCounter,
    };
  }

  static getMentorRank({prefs, mentor}) {
    return getRank(prefs, mentor);
  }

  static meanRank(prefs) {
    return mean(prefs.map(MentorPrefs.getMentorRank));
  }

  static statsOfState(state) {
    if (!state.choices) {
      throw new Error('State does not have property "choices".');
    }
    if (state.score < 0) {
      return `Failed. Score: ${state.score}.\n`;
    }
    return `Score: ${state.score}.\n` +
      `1sts: ${state.choices.map(MentorPrefs.getMentorRank).filter(r => r === 1).length}.\n` +
      `2sts: ${state.choices.map(MentorPrefs.getMentorRank).filter(r => r === 2).length}.\n` +
      `3sts: ${state.choices.map(MentorPrefs.getMentorRank).filter(r => r === 3).length}.\n` +
      `4sts: ${state.choices.map(MentorPrefs.getMentorRank).filter(r => r === 4).length}.\n` +
      `Mean rank: ${MentorPrefs.meanRank(state.choices)}.`;
  }

  static stop(state) {
    const meanRank = MentorPrefs.meanRank(state.prefs);
    return state.score > 0 && meanRank < 1.5;
  }
}

const sampleData = [
  {name: "Jim", prefs: [17, 26, 26, 25]},
  {name: "Jim", prefs: [22, 13, 26, 33]},
  {name: "Jim", prefs: [12, 4, 2, 23]},
  {name: "Jim", prefs: [38, 18, 12, 10]},
  {name: "Jim", prefs: [3, 15, 36, 1]},
  {name: "Jim", prefs: [33, 1, 39, 29]},
  {name: "Jim", prefs: [5, 35, 32, 19]},
  {name: "Jim", prefs: [38, 7, 16, 21]},
  {name: "Jim", prefs: [18, 39, 20, 26]},
  {name: "Jim", prefs: [39, 17, 32, 13]},
  {name: "Jim", prefs: [25, 8, 18, 33]},
  {name: "Jim", prefs: [15, 25, 3, 15]},
  {name: "Jim", prefs: [28, 25, 8, 20]},
  {name: "Jim", prefs: [15, 21, 1, 13]},
  {name: "Jim", prefs: [38, 32, 31, 4]},
  {name: "Jim", prefs: [31, 19, 31, 2]},
  {name: "Jim", prefs: [18, 38, 34, 35]},
  {name: "Jim", prefs: [5, 29, 2, 31]},
  {name: "Jim", prefs: [24, 22, 30, 36]},
  {name: "Jim", prefs: [6, 14, 17, 14]},
];

const trenchB = [
  {name: "Devyani Aggarwal", prefs: [170, 216, 115, 201]},
  {name: "Vivian Armitage", prefs: [52, 130, 192, 71]},
  {name: "Razvan Azamfirei", prefs: [163, 206, 85, 118]},
  {name: "Bibek Basnet", prefs: [163, 209, 84, 45]},
  {name: "Johannes Behringer", prefs: [169, 194, 163, 76]},
  {name: "Surbhi Bharadwaj", prefs: [90, 107, 187, 154]},
  {name: "Aaminah Bhat", prefs: [163, 166, 162, 6]},
  {name: "Urvashi Bhatnagar", prefs: [163, 176, 211, 130]},
  {name: "Hillary Browning", prefs: [79, 56, 116, 118]},
  {name: "Jordan Cozby", prefs: [162, 147, 139, 3]},
  {name: "Megan Crandall", prefs: [201, 162, 71, 9]},
  {name: "Julius Csizmazia", prefs: [163, 179, 80, 194]},
  {name: "Aryssa Damron", prefs: [16, 95, 88, 56]},
  {name: "Reed Dibich", prefs: [204, 101, 28, 181]},
  {name: "Julia Ding", prefs: [185, 124, 186, 197]},
  {name: "Adam Erickson", prefs: [215, 216, 169, 107]},
  {name: "Meng Fan", prefs: [215, 216, 169, 42]},
  {name: "Kin Hang Fung", prefs: [6, 160, 45, 174]},
  {name: "He Gao", prefs: [163, 107, 28, 179]},
  {name: "Topiltzin Gomez", prefs: [165, 113, 119, 35]},
  {name: "Madeline Hoffmann", prefs: [215, 190, 173, 124]},
  {name: "Abdul-Majeed Ibrahim", prefs: [71, 194, 53, 42]},
  {name: "Madhurima Jaiswal", prefs: [201, 163, 167, 151]},
  {name: "Kelsea Jeon", prefs: [160, 145, 116, 217]},
  {name: "Christine Kang", prefs: [49, 214, 69, 88]},
  {name: "Gilad kaufman", prefs: [215, 60, 42, 147]},
  {name: "Rachel Kornbluh", prefs: [182, 197, 97, 136]},
  {name: "Navin Kumar", prefs: [167, 194, 139, 71]},
  {name: "joseph kwon", prefs: [185, 194, 160, 163]},
  {name: "Haleigh Larson", prefs: [19, 93, 171, 197]},
  {name: "Linshu Li", prefs: [163, 71, 215, 107]},
  {name: "Natasha Liggett", prefs: [53, 65, 147, 110]},
  {name: "Michelle Lim", prefs: [215, 188, 184, 107]},
  {name: "Kevin Lin", prefs: [107, 139, 163, 59]},
  {name: "Sabrina Long", prefs: [169, 163, 42, 106]},
  {name: "Lie Ma", prefs: [139, 107, 172, 197]},
  {name: "Michael Mattessich", prefs: [169, 194, 126, 151]},
  {name: "Holly McLaughlin", prefs: [57, 45, 63, 90]},
  {name: "Katie Melbourne", prefs: [209, 161, 162, 202]},
  {name: "Nathan Micon", prefs: [169, 139, 151, 192]},
  {name: "Margaret Moor", prefs: [139, 71, 45, 163]},
  {name: "Anthony (Tony) Oliverio", prefs: [126, 163, 201, 169]},
  {name: "Lun Ou", prefs: [106, 204, 99, 169]},
  {name: "Saee Pansare", prefs: [68, 141, 146, 80]},
  {name: "Sonia Helen Pascale", prefs: [95, 4, 107, 110]},
  {name: "Brunilda Pizarro", prefs: [160, 191, 53, 19]},
  {name: "Daniel Ren", prefs: [170, 216, 115, 201]},
  {name: "Alec Rodriguez", prefs: [104, 85, 19, 74]},
  {name: "Jack Roth", prefs: [90, 148, 161, 216]},
  {name: "Madison Sack", prefs: [216, 42, 167, 115]},
  {name: "Natalya Sanghvi", prefs: [204, 127, 194, 88]},
  {name: "Jennifer Santiago", prefs: [107, 160, 56, 199]},
  {name: "Philippa Smit", prefs: [49, 107, 182, 79]},
  {name: "Audrey Storm", prefs: [193, 88, 207, 217]},
  {name: "Daqi Sun", prefs: [188, 216, 90, 18]},
  {name: "Anand Swaminathan", prefs: [139, 107, 73, 167]},
  {name: "Jessica (Si Jie) Tang", prefs: [185, 124, 19, 197]},
  {name: "modupeola thomas", prefs: [119, 126, 35, 194]},
  {name: "Liana Wang", prefs: [3, 145, 217, 71]},
  {name: "Sonia Wang", prefs: [104, 197, 182, 93]},
  {name: "Katherine Werwie", prefs: [127, 109, 44, ]},
  {name: "Resla Wesonga", prefs: [163, 204, 85, 160]},
  {name: "Kristina Whyte", prefs: [76, 130, 42, 167]},
  {name: "Luwei Xiong", prefs: [145, 209, 162, 217]},
  {name: "Susan Xu", prefs: [216, 169, 115, 194]},
  {name: "Sisi Xue", prefs: [169, 163, 76, ]},
  {name: "Amy Yang", prefs: [184, 197, 76, 167]},
  {name: "Wenyue Yang", prefs: [79, 85, 35, 167]},
  {name: "Hannah Yang", prefs: [71, 110, 119, 204]},
  {name: "Celine Shu Wan Yeap", prefs: [18, 9, 139, 138]},
  {name: "Sarah Yoo", prefs: [71, 129, 204, 210]},
  {name: "yani zeng", prefs: [132, 87, 103, 66]},
  {name: "Jessie Zhao", prefs: [103, 73, 125, 98]},
];

module.exports = MentorPrefs;

// const MentorOptimizer = Optimizer(MentorPrefs);
// MentorOptimizer.optimize(state => console.log(MentorPrefs.statsOfState(state)), () => null, 5, 5, 100, trenchB);
