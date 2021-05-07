// data for all stimuli in the form of a list of JavaScript objects

var practice_stims = 
  [{}]

// Properties of each stimulus:
// audio : "word_vot_num_f0_num.wav"
// poa : lab / cor / dor 
// vot : 1 2 3 4 5 6 7 8 9
// f0 : 1 2 3 4 5 6 7 8 9

trial_stims = []
word_poa = {"pang" : "lab", "tam" : "cor", "kan" : "dor"}
poa_laryngeal = {"lab" : ["방", "빵", "팡"], "cor" : ["담", "땀", "탐"], "dor" : ["간", "깐", "칸"]}
for (const [word, poa] of Object.entries(word_poa)) {
  for (vot = 1; vot < 10; vot++) {
    for (f0 = 1; f0 < 10; f0++) {
      trial_stims.push({
        "audio" : word + "_vot_" + vot + "_f0_" + f0 + ".wav",
        "poa" : poa,
        "word": word,
        "vot": vot,
        "f0": f0})
    }
  }
}