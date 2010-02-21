import tt.TypingTest;
import tt.TypingTestEvent;

import flash.events.Event;

import mx.controls.Alert;
import mx.events.FlexEvent;
import mx.rpc.events.ResultEvent;

private static const PAUSE_LABEL:String = "Pauza";
private static const CONTINUE_LABEL:String = "Wznów";

[Bindable]
private var speed:int = 0;
[Bindable]
private var correctness:int = 0;

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
}

private function onTypingTestActive(event:TypingTestEvent):void {
    isNewTestButtonActive = true;
}

private function onTestResultsUpdate(event:TypingTestEvent):void {
    speed = event.testResults.speed;
    correctness = event.testResults.correctness;
}

private function onTypingTestFinished(event:TypingTestEvent):void {
    Alert.show(event.testResults.toString());
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

private function onSubmitTestResultsServiceResult(event:ResultEvent):void {
    var serviceResponse:String = event.result.toString();
    Alert.show(serviceResponse);
}

private function startNewTypingTest():void {
    getTextService.cancel();
    var params:Object = new Object();
    params.action = "get_text";
    params.text_index = (int)(50 * Math.random());
    getTextService.send(params);
}
