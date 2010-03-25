import tt.mx.TestResultsWindow;
import tt.TestResults;
import tt.TypingTest;
import tt.TypingTestEvent;

import com.hurlant.crypto.hash.HMAC;
import com.hurlant.crypto.hash.IHMAC;
import com.hurlant.crypto.hash.SHA1;
import com.hurlant.util.Hex;

import flash.events.Event;

import mx.events.FlexEvent;
import mx.managers.PopUpManager;
import mx.rpc.events.ResultEvent;

private static const PAUSE_LABEL:String = "Pauza";
private static const CONTINUE_LABEL:String = "Wznów";

[Bindable]
private var speedCPM:Number = 0;
[Bindable]
private var speedWPM:Number = 0;
[Bindable]
private var correctness:Number = 0;

private var isNewTestButtonActive:Boolean = false;
private var hData:String = "";

public static function h(hData:String, hKey:String):String {
    const hmac:IHMAC = new HMAC(new SHA1());
    return Hex.fromArray(hmac.compute(
                Hex.toArray(Hex.fromString(hKey)),
                Hex.toArray(Hex.fromString(hData))));
}

private function init():void {
    initEventListeners();

    new TypingTest(typingCanvas, this);
    startNewTypingTest();
}

private function initEventListeners():void {
    pauseButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onPauseButtonClicked);
    newTestButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onNewTestButtonClicked);
    plCharsCheckBox.addEventListener(
            FlexEvent.BUTTON_DOWN, onPlCharsCheckBoxClicked);

    addEventListener(TypingTestEvent.TYPING_TEST_ACTIVE,
            onTypingTestActive);
    addEventListener(TypingTestEvent.TEST_RESULTS_UPDATE,
            onTestResultsUpdate);
    addEventListener(TypingTestEvent.TYPING_STARTED,
            onTypingStarted);
    addEventListener(TypingTestEvent.TYPING_TEST_FINISHED,
            onTypingTestFinished);
    addEventListener(TypingTestEvent.PAUSE, onPause);
    addEventListener(TypingTestEvent.CONTINUE, onContinue);

    getTextService.addEventListener("result", onGetTextServiceResult);
}

private function onPauseButtonClicked(event:Event):void {
    if (event.currentTarget.label == PAUSE_LABEL) {
        typingCanvas.dispatchEvent(
                new TypingTestEvent(TypingTestEvent.PAUSE));
    } else {
        typingCanvas.dispatchEvent(
                new TypingTestEvent(TypingTestEvent.CONTINUE));
    }
    callLater(typingCanvas.setFocus);
}

private function onNewTestButtonClicked(event:Event):void {
    if (isNewTestButtonActive) {
        pauseButton.label = PAUSE_LABEL;
        startNewTypingTest();
    }
    callLater(typingCanvas.setFocus);
    pauseButton.enabled = false;
}

private function onPlCharsCheckBoxClicked(event:Event):void {
    typingCanvas.dispatchEvent(
            new TypingTestEvent(TypingTestEvent.PL_CHARS_CHANGE));
    callLater(typingCanvas.setFocus);
}

private function onTypingTestActive(event:TypingTestEvent):void {
    isNewTestButtonActive = true;
}

private function onTypingStarted(event:TypingTestEvent):void {
    pauseButton.enabled = true;
}

private function onTestResultsUpdate(event:TypingTestEvent):void {
    speedCPM = event.testResults.realSpeed;
    speedWPM = event.testResults.realSpeedWPM;
    correctness = event.testResults.correctness;
}

private function onTypingTestFinished(event:TypingTestEvent):void {
    var resultsWindow:TestResultsWindow = PopUpManager.createPopUp(
            this, TestResultsWindow, true) as TestResultsWindow;
    resultsWindow.testResults = event.testResults;
    PopUpManager.centerPopUp(resultsWindow);
    resultsWindow.setFocus();
    pauseButton.enabled = false;

    submitTestResults(event.testResults);
}

private function onPause(event:TypingTestEvent):void {
    pauseButton.label = CONTINUE_LABEL;
}

private function onContinue(event:TypingTestEvent):void {
    pauseButton.label = PAUSE_LABEL;
}

private function onGetTextServiceResult(event:ResultEvent):void {
    const result:XML = XML(event.result);
    typingCanvas.dispatchEvent(
            new TypingTestEvent(TypingTestEvent.NEW_TYPING_TEST, result.text));
    hData = result.hData;
    getTextService.showBusyCursor = true;
}

private function startNewTypingTest():void {
    getTextService.cancel();
    getTextService.send();
}

private function submitTestResults(testResults:TestResults):void {
    submitTestResultsService.cancel();
    var params:Object = new Object();
    params.speed = testResults.realSpeed.toFixed(1);
    params.mistakes = testResults.mistakesCount;
    params.corrections = testResults.correctionsCount;
    params.plChars = testResults.plChars;
    params.correctChars =
        testResults.writtenCharsCount - testResults.mistakesCount;
    params.minutes = int(testResults.timeMinutes);
    params.seconds = int(testResults.timeSeconds) % 60;
    params.timeVerifier = testResults.timeSecondsVerifier;
    params.h = h(hData + ':'
            + params.speed + ':'
            + params.mistakes + ':'
            + params.corrections + ':'
            + params.plChars + ':'
            + params.minutes + ':'
            + params.seconds + ':'
            + params.timeVerifier,
            "secret1");
    submitTestResultsService.send(params);
}
