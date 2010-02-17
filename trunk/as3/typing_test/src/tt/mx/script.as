import tt.TypingTest;

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

    addEventListener("pause", onPause);
    addEventListener("continue", onContinue);
}

private function onPauseButtonClicked(event:Event):void {
    if (event.currentTarget.label == PAUSE) {
        dispatchEvent(new Event("pause"));
    } else {
        dispatchEvent(new Event("continue"));
    }
}

private function onPause(event:Event):void {
    pauseButton.label = CONTINUE;
}

private function onContinue(event:Event):void {
    pauseButton.label = PAUSE;
}

private function onGetTextServiceResult(event:ResultEvent):void {
    var serviceResponse:String = event.result.toString();
    Alert.show(serviceResponse);
}

private function onSubmitTestResultService(event:ResultEvent):void {
    var serviceResponse:String = event.result.toString();
    Alert.show(serviceResponse);
}

private function getNewText():void {
    getTextService.cancel();
    var params:Object = new Object();
    params.text_index = 25;
    getTextService.send(params);
}
