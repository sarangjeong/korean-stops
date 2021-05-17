function make_slides(f) {
  var slides = {};

  // set up initial slide
  slides.i0 = slide({
    name: "i0",
    start: function() {
      exp.startT = Date.now();
    }
  });

  // set up audio check slide
  // TODO : record wrong answers
  slides.check = slide({ // instead of audio_check
    name : "check", // instead of audio_check
    start: function() {
      $('.err').hide();
      // $('.question').hide();
      document.getElementById("audio_check").play();
        // setTimeout(function(){
          // $('.question').show();
        // },1500);
    },
    button : function() {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "y") {
        this.log_responses();
        exp.go();
      }
      else{
        $('.err').show();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "id": "check", // instead of "audio_check"
          "response": this.radio,
          "vot": "",
          "f0": "",
          // "image" : "",
          // "audio" : "",
      });
    }
  });


  // set up the first example slide
  slides.example1 = slide({
    name: "example1",

    // this is executed when the slide is shown
    start: function() {
      // hide error message
      $('.err').hide();
    },

    // this is executed when the participant clicks the "Continue button"
    button: function() {
      // read in the value of the selected radio button
      this.radio = $("input[name='word']:checked").val();
      // check whether the participant selected a reasonable value (i.e, 5, 6, or 7)
      if (this.radio == "tense") {
        // log response
        this.log_responses();
        // continue to next slide
        exp.go();
      } else {
        // participant gave non-reasonable response --> show error message
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function() {
      // add response to exp.data_trials
      // this data will be submitted at the end of the experiment
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "id": "example1",
        "response": this.radio,
        "vot": "",
        "f0": "",
        // "strangeSentence": "",
        // "sentence": "",
      });
    },
  });
  console.log(exp.data_trials)

  // set up slide for second example trial
  slides.example2 = slide({
    name: "example2",

    start: function() {
      // hide error message
      $(".err").hide();
    },

    // handle button click
    button: function() {
      this.radio = $("input[name='word']:checked").val();
      if (this.radio == "asp") {
        this.log_responses();
        exp.go();
      } else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "id": "example2",
        "response": this.radio,
        "vot": "",
        "f0": "",
        // "strangeSentence": "",
        // "sentence": "",
      });
    }
  });
  console.log(exp.data_trials)

  // set up slide for third example trial
  slides.example3 = slide({
    name: "example3",

    start: function() {
      // hide error message
      $(".err").hide();
    },

    // handle button click
    button: function() {
      this.radio = $("input[name='word']:checked").val();
      if (this.radio == "lenis") {
        this.log_responses();
        exp.go();
      } else {
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "id": "example3",
        "response": this.radio,
        "vot": "",
        "f0": "",
        // "strangeSentence": "",
        // "sentence": "",
      });
    }
  });
  console.log(exp.data_trials)

  // set up slide with instructions for main experiment
  slides.startExp = slide({
    name: "startExp",
    start: function() {
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.trial1 = slide({
    name: "trial1",

    present: _.shuffle(exp.stimuli[exp.trial_poa[0]]),
    present_handle : function(stim) {

      // unselect all radio buttons (Leyla)
      $('#lenis1').empty()
      $('#tense1').empty()
      $('#asp1').empty()

      // store stimulus data
      this.stim = stim;

      // TODO : lenis, tense, aps radios --> depending on the stim's poa, change their "labels" into 방빵팡 / 담땀탐 / 간깐칸

      // for option 1 (lenis)
      $('#lenis1').append(
            $('<input>').prop({
                type: 'radio',
                id: this.stim.poa + "lenis",
                value: "lenis",
                name: "word"
            })
        ).append(
            $('<label>').prop({
                for: this.stim.poa + "lenis"
            }).html(poa_laryngeal[this.stim.poa][0]))

      // for option 2 (tense)
      $('#tense1').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "tense",
            value: "tense",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "tense"
          }).html(poa_laryngeal[this.stim.poa][1]))

      // for option 3 (aspirated)
      $('#asp1').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "asp",
            value: "asp",
            name: "word"
        })
       ).append(
        $('<label>').prop({
            for: this.stim.poa + "asp"
          }).html(poa_laryngeal[this.stim.poa][2]))

      var aud = document.getElementById("stim1");
      aud.src = "audio/"+stim.audio;
      console.log("audio source:",aud.src)
      aud.load();
      aud.play();

      $(".err").hide();

    },

    // handle click on "Continue" button
    button: function() {
      this.radio = $("input[name='word']:checked").val();
      // this.strange = $("#check-strange:checked").val() === undefined ? 0 : 1;
      if (this.radio) {
        this.log_responses();
        // exp.go(); //use exp.go() if and only if there is no "present"ed data, ie no list of stimuli.
        _stream.apply(this); //use _stream.apply(this) if there is a list of "present" stimuli to rotate through
      } else {
        $('.err').show();
      }
    },

    // save response
    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase, //exp.phase is a built-in trial number tracker
        "id": this.stim.audio,
        "response": this.radio,
        "word": this.stim.word,
        "poa": this.stim.poa,
        "vot": this.stim.vot,
        "f0": this.stim.f0
      });
    },
  });

  // set up slide for break
  // TODO : tell them how many blocks are left
  slides.break1 = slide({
    name: "break1",
    start: function() {
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.trial2 = slide({
    name: "trial2",

    present: _.shuffle(exp.stimuli[exp.trial_poa[1]]),
    present_handle : function(stim) {

      // unselect all radio buttons (Leyla)
      $('#lenis2').empty()
      $('#tense2').empty()
      $('#asp2').empty()

      // store stimulus data
      this.stim = stim;

      // TODO : lenis, tense, aps radios --> depending on the stim's poa, change their "labels" into 방빵팡 / 담땀탐 / 간깐칸

      // for option 1 (lenis)
      $('#lenis2').append(
            $('<input>').prop({
                type: 'radio',
                id: this.stim.poa + "lenis",
                value: "lenis",
                name: "word"
            })
        ).append(
            $('<label>').prop({
                for: this.stim.poa + "lenis"
            }).html(poa_laryngeal[this.stim.poa][0]))

      // for option 2 (tense)
      $('#tense2').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "tense",
            value: "tense",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "tense"
          }).html(poa_laryngeal[this.stim.poa][1]))

      // for option 3 (aspirated)
      $('#asp2').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "asp",
            value: "asp",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "asp"
          }).html(poa_laryngeal[this.stim.poa][2]))
          
      var aud = document.getElementById("stim2");
      aud.src = "audio/"+stim.audio;
      console.log("audio source:",aud.src)
      aud.load();
      aud.play();

      $(".err").hide();

    },

    // handle click on "Continue" button
    button: function() {
      this.radio = $("input[name='word']:checked").val();
      // this.strange = $("#check-strange:checked").val() === undefined ? 0 : 1;
      if (this.radio) {
        this.log_responses();
        // exp.go(); //use exp.go() if and only if there is no "present"ed data, ie no list of stimuli.
        _stream.apply(this); //use _stream.apply(this) if there is a list of "present" stimuli to rotate through
      } else {
        $('.err').show();
      }
    },

    // save response
    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase, //exp.phase is a built-in trial number tracker
        "id": this.stim.audio,
        "response": this.radio,
        "word": this.stim.word,
        "poa": this.stim.poa,
        "vot": this.stim.vot,
        "f0": this.stim.f0
      });
    },
  });

  // set up slide for break
  // TODO : tell them how many blocks are left
  slides.break2 = slide({
    name: "break2",
    start: function() {
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.trial3 = slide({
    name: "trial3",

    present: _.shuffle(exp.stimuli[exp.trial_poa[2]]),
    present_handle : function(stim) {

      // unselect all radio buttons (Leyla)
      $('#lenis3').empty()
      $('#tense3').empty()
      $('#asp3').empty()

      // store stimulus data
      this.stim = stim;

      // TODO : lenis, tense, aps radios --> depending on the stim's poa, change their "labels" into 방빵팡 / 담땀탐 / 간깐칸

      // for option 1 (lenis)
      $('#lenis3').append(
            $('<input>').prop({
                type: 'radio',
                id: this.stim.poa + "lenis",
                value: "lenis",
                name: "word"
            })
        ).append(
            $('<label>').prop({
                for: this.stim.poa + "lenis"
            }).html(poa_laryngeal[this.stim.poa][0]))

      // for option 2 (tense)
      $('#tense3').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "tense",
            value: "tense",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "tense"
          }).html(poa_laryngeal[this.stim.poa][1]))

      // for option 3 (aspirated)
      $('#asp3').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "asp",
            value: "asp",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "asp"
          }).html(poa_laryngeal[this.stim.poa][2]))

      var aud = document.getElementById("stim3");
      aud.src = "audio/"+stim.audio;
      console.log("audio source:",aud.src)
      aud.load();
      aud.play();

      $(".err").hide();

    },

    // handle click on "Continue" button
    button: function() {
      this.radio = $("input[name='word']:checked").val();
      // this.strange = $("#check-strange:checked").val() === undefined ? 0 : 1;
      if (this.radio) {
        this.log_responses();
        // exp.go(); //use exp.go() if and only if there is no "present"ed data, ie no list of stimuli.
        _stream.apply(this); //use _stream.apply(this) if there is a list of "present" stimuli to rotate through
      } else {
        $('.err').show();
      }
    },

    // save response
    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase, //exp.phase is a built-in trial number tracker
        "id": this.stim.audio,
        "response": this.radio,
        "word": this.stim.word,
        "poa": this.stim.poa,
        "vot": this.stim.vot,
        "f0": this.stim.f0
      });
    },
  });

  // slide to collect subject information
  slides.subj_info = slide({
    name: "subj_info",
    submit: function(e) {
      exp.subj_data = {
        language: $("#language").val(),
        enjoyment: $("#enjoyment").val(),
        asses: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        education: $("#education").val(),
        fairprice: $("#fairprice").val(),
        comments: $("#comments").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  // fianl slide
  slides.thanks = slide({
    name: "thanks",
    start: function() {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials,
        "system": exp.system,
        "condition": exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000
      };
      proliferate.submit(exp.data);
    }
  });

  return slides;
}

/// initialize experiment
function init() {

  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = trial_stims;

  exp.stimuli = stimuli // _.shuffle(stimuli); //call _.shuffle(stimuli) to randomize the order;
  
  // determine which poa (방빵팡/담땀탐/간깐칸) to put in each block
  exp.trial_poa = ['lab', 'lab', 'lab'] // _.shuffle(Object.keys(stimuli)); // shuffle poas of trial

  console.log(exp.stimuli) //I added this during tutorial
  exp.n_trials = exp.stimuli.length;

  // exp.condition = _.sample(["context", "no-context"]); //can randomize between subjects conditions here

  // TODO : record speaker or earphone; or ask about it in questionnaire
  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width
  };

  //blocks of the experiment:
  exp.structure = [
    "i0",
    "check", // instead of "audio_check"
    "example1",
    "example2",
    "example3",
    "startExp",
    "trial1", // 방 빵 팡
    "break1",
    "trial2", // 간 깐 칸
    "break2",
    "trial3", // 담 땀 탐
    "subj_info",
    "thanks"
  ];

  exp.data_trials = [];

  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length();
  //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  $("#start_button").click(function() {
    exp.go();
  });

  exp.go(); //show first slide
}
