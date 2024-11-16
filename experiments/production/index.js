let timeline = [
    consentFormSlide,
    overallInstructionSlide,
    ...createSession(1),
    ...createSession(2),
    ...createSession(3, false),
    thanksSlide
]; // Replaced experimentInstruction with overallInstruction

// Function to get the query parameter by name
function getQueryParameter(name) {
    const urlParams = new URLSearchParams(window.location.search);
    return urlParams.get(name);
}

// Retrieve the participant_id from the URL
const participant_id = getQueryParameter('participant_id');

jsPsych.init({
    timeline: timeline,
    show_progress_bar: true,
    auto_update_progress_bar: true,
    message_progress_bar: "진행 상황",
    on_finish: function () {
        proliferate.submit({
            "experiment_data": jsPsych.data.get().json() 
        });
    }
});
