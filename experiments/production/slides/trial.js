// Helper function to convert numbers to Korean ordinal words
function convertNumberToKoreanOrdinal(num) {
    switch(num) {
        case 1: return "첫번째";
        case 2: return "두번째";
        case 3: return "세번째";
        case 4: return "네번째";
        case 5: return "다섯번째";
        default: return `${num}번째`; // Fallback for numbers beyond 5
    }
}

// Function to create a session block
function createSession(sessionNumber) {
    // Session instruction
    var sessionInstruction = {
        type: "html-button-response",
        stimulus: `
        <div style="text-align: left;">
            <h1>세션 ${sessionNumber}</h1>
    
            <h2>주의 사항</h2>
            <ol style="padding-left: 20px; font-size: 1em;">
                <li>실험을 시작하기 전에 <strong>주변의 소음을 차단</strong>해 주세요.</li>
                <li>스마트폰에서 <strong>음성메모 녹음</strong>을 시작해 주세요. 녹음기를 입에서 약 <strong>10-20cm</strong> 거리에 두세요. (다른 녹음 장비가 있으실 경우 그것을 사용하셔도 좋습니다.)</li>
                <li>단어를 보고 나서 소리내어 읽기 전 <strong>1초 정도 간격</strong>을 두세요.</li>
                <li>단어를 읽은 뒤 <strong>1초 정도 간격</strong>을 두고 다음 슬라이드로 넘어가세요. 마우스를 좌클릭하거나 스페이스바를 누르면 다음 슬라이드로 넘어갑니다.</li>
                <li>단어를 <strong>나열하듯 쭉 읽지 마시고</strong>, 각 단어를 독립적으로 읽어 주세요.</li>
            </ol>
    
            <p>준비가 되셨으면 다음 슬라이드로 넘어가 실험을 시작해 주세요.</p>
        </div>`,
        choices: ['시작']
    };
    

    // Session trial (customize this for each session as needed)
    const koreanWords = [
        "각", "간", "곡", "공", "기", "김", "깍", "깐", "꼭", "꽁", "끼", "낙", "난", "날",
        "남", "납", "낫", "녹", "논", "놈", "놋", "농", "님", "닥", "단", "독", "동", "디",
        "딱", "딴", "똑", "똥", "띠", "막", "만", "말", "맘", "맛", "망", "목", "몸", "못",
        "미", "박", "반", "복", "봉", "비", "빡", "빤", "뽁", "뽕", "삐", "속", "손", "솔",
        "솜", "솥", "시", "씨", "악", "안", "알", "암", "앞", "옥", "옷", "이", "잣", "좀",
        "짐", "착", "촉", "촌", "침", "칵", "칸", "콕", "키", "탁", "탄", "톡", "통", "티",
        "팍", "판", "폭", "퐁", "피", "학", "혹", "혼", "힘", "끼", "기", "키", "삐", "비",
        "피", "띠", "디", "티"
    ];

    // With sessionNumber being seed, shuffle koreanWords
    var shuffledKoreanWords = jsPsych.randomization.repeat(koreanWords, 1);

    var sessionTrials = shuffledKoreanWords.map(word => {
        return {
            type: "html-button-response",
            stimulus: `<h1 style="font-size: 10em;">${word}</h1>`,
            choices: ['다음']
        }
    })

    // Session end
    var sessionEnd = {
        type: "html-button-response",
        stimulus: `
            <div>
                <h1>${convertNumberToKoreanOrdinal(sessionNumber)} 세션이 끝났습니다.</h1>
                <p>녹음을 중지해 주세요.</p>
                <p>잠시 휴식을 취하셔도 됩니다.</p>
                <p>준비가 되시면 다음 슬라이드로 넘어가 다음 세션을 시작해 주세요.</p>
            </div>`,
        choices: ['다음']
    };

    // Return the session as an array to be added to the timeline
    return [sessionInstruction, ...sessionTrials, sessionEnd];
}