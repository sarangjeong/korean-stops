function make_trial_slide(set_index) {
  // main experiment, block 1
  return slide({
    name: "trial",
    present: _.shuffle(exp.stimuli[exp.trial_poa[set_index]]),
    present_handle : function(stim) {

      // unselect all radio buttons (Leyla)
      $('#lenis').empty()
      $('#tense').empty()
      $('#asp').empty()
      $(".err").hide()

      // store stimulus data
      this.stim = stim;

      // TODO : lenis, tense, aps radios --> depending on the stim's poa, change their "labels" into 방빵팡 / 담땀탐 / 간깐칸

      // for option 1 (lenis)
      $('#lenis').append(
            $('<input>').prop({
                type: 'radio',
                id: this.stim.poa + "lenis",
                value: "lenis",
                name: "word"
            })
        ).append(
            $('<label>').prop({
                for: this.stim.poa + "lenis"
            }).html(/*"1. " + */poa_laryngeal[this.stim.poa][0]))

      // for option 2 (tense)
      $('#tense').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "tense",
            value: "tense",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "tense"
          }).html(/*"2. " + */poa_laryngeal[this.stim.poa][1]))

      // for option 3 (aspirated)
      $('#asp').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "asp",
            value: "asp",
            name: "word"
        })
       ).append(
        $('<label>').prop({
            for: this.stim.poa + "asp"
          }).html(/*"3. " + */poa_laryngeal[this.stim.poa][2]))

      var aud = document.getElementById("stim");
      aud.src = "audio/"+stim.audio;
      console.log("audio source:",aud.src)
      aud.load();
      // aud.play();
      setTimeout(function(){ 
        aud.play(); 
        }, 300)


      $(".err").hide();

    },

    // handle click on "Continue" button
    button: function() {
      this.radio = $("input[name='word']:checked");
      // this.strange = $("#check-strange:checked").val() === undefined ? 0 : 1;
      if (this.radio.val()) {
        this.log_responses();
        this.radio.prop("checked", false)
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
}

function make_break_slide() {
  return slide({
    name: "break",
    start: function() {
      exp.trial_timestamp.push(Date.now())
    },
    button: function() {
      exp.trial_timestamp.push(Date.now())
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });
}

function make_slides(f) {
  var slides = {};

  // set up initial slide
  slides.i0 = slide({
    name: "i0",
    start: function() {
      exp.startT = Date.now();
    }
  });

  // set up audio & Korean check slide 1
  slides.check1 = slide({ // instead of audio_check
    name : "check1", // instead of audio_check
    start: function() {
      $('.err').hide();
      $('.failure').hide();
      // $('.question').hide();
      document.getElementById("audio_check1").play();
        // setTimeout(function(){
          // $('.question').show();
        // },4500);
    },
    button : function() {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "true") {
        this.log_responses();

        if (exp.check_fails > 1) {
          $('.err').hide();
          $('.failure').show();
          setTimeout(function(){
            close();
          },5000);
        }
        else {
          exp.go();
        }
      }
      else{
        $('.err').show();
        exp.check_fails += 1;
        this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "id": "check", // instead of "audio_check"
          "response": this.radio,
          "word":  "check1"
          // "vot": "",
          // "f0": ""
      });
    }
  });

  // set up audio & Korean check slide 2
  slides.check2 = slide({ // instead of audio_check
    name : "check2", // instead of audio_check
    start: function() {
      $('.err').hide();
      $('.failure').hide();
      // $('.question').hide();
      document.getElementById("audio_check2").play();
        // setTimeout(function(){
          // $('.question').show();
        // },4000);
    },
    button : function() {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "true") {
        this.log_responses();

        if (exp.check_fails > 1) {
          $('.failure').show();
          setTimeout(function(){
            close();
          },5000);
        }
        else {
          exp.go();
        }
      }
      else{
        $('.err').show();
        exp.check_fails += 1;
        this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "id": "check", 
          "response": this.radio,
          "word":  "check2"
          // "vot": "",
          // "f0": ""
      });
    }
  });

  // set up audio & Korean check slide 3
  slides.check3 = slide({ // instead of audio_check
    name : "check3", // instead of audio_check
    start: function() {
      $('.err').hide();
      $('.failure').hide();
      // $('.question').hide();
      document.getElementById("audio_check3").play();
        // setTimeout(function(){
          // $('.question').show();
        // },5000);
    },
    button : function() {
      this.radio = $("input[name='number']:checked").val();
      if (this.radio == "true") {
        this.log_responses();

        if (exp.check_fails > 1) {
          $('.failure').show();
          setTimeout(function(){
            close();
          },5000);
        }
        else {
          exp.go();
        }
      }
      else{
        $('.err').show();
        exp.check_fails += 1;
        this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
          "slide_number_in_experiment": exp.phase,
          "id": "check", 
          "response": this.radio,
          "word":  "check3"
          // "vot": "",
          // "f0": ""
      });
    }
  });

  // set up slide with instructions for practice
  slides.startPractice = slide({
    name: "startPractice",
    start: function() {
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  // set up practice slide
  slides.practice = slide({
    name: "practice",

    present: practice_stims,
    present_handle : function(stim) {

      // unselect all radio buttons (Leyla)
      $('#lenis_p').empty()
      $('#tense_p').empty()
      $('#asp_p').empty()
      $(".err").hide()

      // store stimulus data
      this.stim = stim;

      // for option 1 (lenis)
      $('#lenis_p').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "lenis",
            value: "lenis",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "lenis"
        }).html(/*"1. " + */poa_laryngeal[this.stim.poa][0]))

      // for option 2 (tense)
      $('#tense_p').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "tense",
            value: "tense",
            name: "word"
        })
      ).append(
        $('<label>').prop({
            for: this.stim.poa + "tense"
          }).html(/*"2. " + */poa_laryngeal[this.stim.poa][1]))

      // for option 3 (aspirated)
      $('#asp_p').append(
        $('<input>').prop({
            type: 'radio',
            id: this.stim.poa + "asp",
            value: "asp",
            name: "word"
        })
        ).append(
        $('<label>').prop({
            for: this.stim.poa + "asp"
          }).html(/*"3. " + */poa_laryngeal[this.stim.poa][2]))
  
      var aud = document.getElementById("stim_p");
      aud.src = "audio/"+stim.audio;
      console.log("audio source:",aud.src)
      aud.load();
      // aud.play();
      setTimeout(function(){ 
        aud.play(); 
        }, 300)

      $(".err").hide();

    },

    button: function() {
      this.radio = $("input[name='word']:checked");
      if (this.radio.val()) {
        this.log_responses();
        this.radio.prop("checked", false)
        _stream.apply(this);
      } else {
        $('.err').show();
      }
    },

    log_responses: function() {
      exp.data_trials.push({
        "slide_number_in_experiment": exp.phase,
        "id": "practice",
        "response": this.radio,
        "word": this.stim.word,
        "poa": this.stim.poa,
        "vot": this.stim.vot,
        "f0": this.stim.f0
      });
    },
  });

  // set up slide with instructions for main experiment
  slides.startExp = slide({
    name: "startExp",
    start: function() {
    },
    button: function() {
      exp.trial_timestamp = []
      exp.trial_timestamp.push(Date.now())
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  for (let i = 1; i < exp.number_of_trial_set; i++) {
    slides['trial' + i.toString()] = make_trial_slide(i-1);
    slides['break' + i.toString()] = make_break_slide();
  }
  slides['trial' + exp.number_of_trial_set.toString()] = make_trial_slide(exp.number_of_trial_set-1);
  
  // slide to collect subject information
  slides.subj_info = slide({
    name: "subj_info",
    start: function() {
      exp.trial_timestamp.push(Date.now())
    },
    submit: function(e) {
      exp.subj_data = {
        language: $("#language").val(),
        language_parents: $("#language_parents").val(),
        language_other: $("#language_other").val(),
        impairment: $("#impairment").val(),
        equipment: $("#equipment").val(),
        enjoyment: $("#enjoyment").val(),
        assess: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        fairprice: $("#fairprice").val(),
        comments: $("#comments").val()
      };
      
      $(".err").hide();

      if (
        exp.subj_data["language"] == "" || 
        exp.subj_data["language_parents"] == "" || 
        exp.subj_data["language_other"] == "" || 
        exp.subj_data["impairment"] == "-1" ||
        exp.subj_data["gender"] == "" || 
        exp.subj_data["age"] == "") {
        $('.err').show();
      } else {
        exp.go(); //use exp.go() if and only if there is no "present"ed data, ie no list of stimuli.
      }
      
    }
  });

  // final slide
  slides.thanks = slide({
    name: "thanks",
    start: function() {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials, // what is this???
        "system": exp.system,
        "condition": exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000,
      };

      for (let i = 0; i < exp.trial_timestamp.length-1; i+=2) {
        exp.data['trial_' + (Math.floor(i/2)+1).toString() + '_time_in_minutes'] = (exp.trial_timestamp[i+1] - exp.trial_timestamp[i]) / 60000;
      }

      proliferate.submit(exp.data); // audio check and practice data???
    }
  });

  return slides;
}

/// initialize experiment
function init() {

  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = trial_stims;

  exp.stimuli = stimuli; // _.shuffle(stimuli); //call _.shuffle(stimuli) to randomize the order;
  
  exp.check_fails = 0;
  
  // determine which poa (방빵팡/담땀탐/간깐칸) to put in each block
  exp.trial_poa = _.shuffle(['lab', 'lab', 'lab', 'cor', 'cor', 'cor', 'dor', 'dor', 'dor']) // shuffle poas of trial

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

  exp.number_of_trial_set = exp.trial_poa.length

  //blocks of the experiment:
  exp.structure = [
    "i0",
    "check1", // instead of "audio_check"
    "check2",
    "check3",
    "startPractice",
    "practice",
    "startExp"];

  for (let i = 1; i < exp.number_of_trial_set; i++) {
    exp.structure.push("trial" + i.toString());
    exp.structure.push("break" + i.toString());
  }
  exp.structure.push("trial" + exp.number_of_trial_set.toString());
  exp.structure.push("subj_info")
  exp.structure.push("thanks")

  exp.data_trials = [];

  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length();
  //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  // use keyboard to chose options and go next page
  // TODO : fix bug error message does not go away when using keyboard
  document.addEventListener('keyup', function(e){
    const keyCode = e.key;
      num_radio_elements = $("input[name='word']").length
      trials = []
      for (let i = 1; i < exp.number_of_trial_set; i++) {
        trials.push("trial" + i.toString());
      }

      if((['practice'] + trials).includes(exp.structure[exp.slideIndex])) {
        if(keyCode == '1') {
          $("input[value='lenis']").prop("checked", true);
        }
        else if(keyCode == '2') {
          $("input[name='word'][value='tense']").prop("checked", true);
        }
        else if(keyCode == '3') {
          $("input[name='word'][value='asp']").prop("checked", true);
        }
        else if (keyCode == 'Enter') {
          exp.slides[exp.structure[exp.slideIndex]].button();
        }
        // press any key, then it shows in the console the data collected so far
        else if (keyCode == 'p') {
          console.log(exp.data_trials);
        }
      }
  });

  $("#start_button").click(function() {
    exp.go();
  });

  exp.go(); //show first slide
}
