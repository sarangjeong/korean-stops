// data for all stimuli in the form of a list of JavaScript objects

var practice_stims = 
  [{}]

// Properties of each stimulus:
// audio : "word_vot_num_f0_num.wav"
// poa : lab / cor / dor 
// vot : 1 2 3 4 5 6 7 8
// f0 : 1 2 3 4 5 6 7 8

trial_stims = {"lab" : [], "cor" : [], "dor" : []}
word_poa = {"pang" : "lab", "tam" : "cor", "kan" : "dor"}
poa_laryngeal = {"lab" : ["방", "빵", "팡"], "cor" : ["담", "땀", "탐"], "dor" : ["간", "깐", "칸"]}
for (const [word, poa] of Object.entries(word_poa)) {
  for (vot = 1; vot < 9; vot++) {
    for (f0 = 1; f0 < 9; f0++) {
      trial_stims[poa].push({
        "audio" : word + "_F0_" + f0 + "_VOT_" + vot + ".wav",
        "poa" : poa,
        "word": word,
        "vot": vot,
        "f0": f0})
    }
  }
}
console.log(trial_stims['lab']);