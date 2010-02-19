import tt.TypingTest;
import tt.TypingTestEvent;

import flash.events.Event;

import mx.controls.Alert;
import mx.events.FlexEvent;
import mx.rpc.events.ResultEvent;


private static const PAUSE:String = "Pauza";
private static const CONTINUE:String = "Wznów";

[Bindable]
private var speed:int = 0;
[Bindable]
private var correctness:int = 0;

private function init():void {
    initEventListeners();

    new TypingTest(typingCanvas, this);
}

private function initEventListeners():void {
    pauseButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onPauseButtonClicked);
    newTestButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onNewTestButtonClicked);

    addEventListener(TypingTestEvent.PAUSE, onPause);
    addEventListener(TypingTestEvent.CONTINUE, onContinue);

    getTextService.addEventListener("result", onGetTextServiceResult);
}

private function onPauseButtonClicked(event:Event):void {
    if (event.currentTarget.label == PAUSE) {
        typingCanvas.dispatchEvent(
                new TypingTestEvent(TypingTestEvent.PAUSE));
    } else {
        typingCanvas.dispatchEvent(
                new TypingTestEvent(TypingTestEvent.CONTINUE));
    }
    callLater(typingCanvas.setFocus);
}

private function onNewTestButtonClicked(event:Event):void {
    getTextService.cancel();
    var params:Object = new Object();
    params.action = "get_text";
    params.text_index = (int)(50 * Math.random());
    getTextService.send(params);
    callLater(typingCanvas.setFocus);
}

private function onPause(event:Event):void {
    pauseButton.label = CONTINUE;
}

private function onContinue(event:Event):void {
    pauseButton.label = PAUSE;
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
