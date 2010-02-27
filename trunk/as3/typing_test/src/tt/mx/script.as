import tt.mx.TestResultsWindow;
import tt.TypingTest;
import tt.TypingTestEvent;

import flash.events.Event;

import mx.containers.TitleWindow;
import mx.controls.Text;
import mx.events.CloseEvent;
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

private function init():void {
    initEventListeners();

    new TypingTest(typingCanvas, this);
    //startNewTypingTest(); // DEBUG
}

private function initEventListeners():void {
    pauseButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onPauseButtonClicked);
    newTestButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onNewTestButtonClicked);

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
        startNewTypingTest();
    }
    callLater(typingCanvas.setFocus);
    pauseButton.enabled = false;
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
    resultsWindow.htmlText = event.testResults.toHTMLString();
    PopUpManager.centerPopUp(resultsWindow);
    resultsWindow.setFocus();
    pauseButton.enabled = false;
}

private function onPause(event:Event):void {
    pauseButton.label = CONTINUE_LABEL;
}

private function onContinue(event:Event):void {
    pauseButton.label = PAUSE_LABEL;
}

private function onGetTextServiceResult(event:ResultEvent):void {
    var text:String = event.result.toString();
    typingCanvas.dispatchEvent(
            new TypingTestEvent(TypingTestEvent.NEW_TYPING_TEST, text));
}

private function startNewTypingTest():void {
    getTextService.cancel();
    getTextService.send();
}
