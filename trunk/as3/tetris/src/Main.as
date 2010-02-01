package {
    import flash.utils.Timer;
    import flash.events.TimerEvent;
    import flash.events.KeyboardEvent;
    import flash.display.Sprite;
    import flash.ui.Keyboard;

    public class Main extends Sprite {

        private var xCoord:int = 10;

        public function Main() {
            stage.addEventListener(KeyboardEvent.KEY_UP, keyHandler);
            var timer:Timer = new Timer(1000, 8);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();
        }

        public function timerHandler(event:TimerEvent):void {
            trace("timerHandler: " + event);
            trace("position: " + xCoord);
        }

        private function keyHandler(event:KeyboardEvent):void {
            switch (event.keyCode) {
                case Keyboard.LEFT:
                    xCoord--;
                    trace("strzałka w lewo");
                    break;
                case Keyboard.RIGHT:
                    xCoord++;
                    trace("strzałka w prawo");
                    break;
            }
        }
    }
}
