let timeline = [
    consentFormSlide,
    overallInstructionSlide,
    // ...createSession(1, false),
    thanksSlide
    // ...createSession(2),
    // ...createSession(3)
]; // Replaced experimentInstruction with overallInstruction

jsPsych.init({
    timeline: timeline,
    show_progress_bar: true,
    auto_update_progress_bar: true,
    message_progress_bar: "진행 상황",
    on_finish: function () {
        proliferate.submit({ "trials": "test" });
    }
});
