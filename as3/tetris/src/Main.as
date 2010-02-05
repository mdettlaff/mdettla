package {

    import tetris.Tetromino;
    import tetris.TetrominoCreator;

    import flash.utils.Timer;
    import flash.events.Event;
    import flash.events.TimerEvent;
    import flash.events.KeyboardEvent;
    import flash.display.Sprite;
    import flash.ui.Keyboard;

    public class Main extends Sprite {

        private var tetromino:Tetromino;

        public function Main() {
            stage.addEventListener(Event.ENTER_FRAME, init);

            stage.addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);

            var timer:Timer = new Timer(1000, 0);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();
        }

        private function init(event:Event):void {
            stage.removeEventListener(Event.ENTER_FRAME, init);
            var tetrominoFactory:TetrominoCreator = new TetrominoCreator();
            tetromino =
                tetrominoFactory.addTetromino(TetrominoCreator.L, this.stage);
        }

        private function timerHandler(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function keyHandler(event:KeyboardEvent):void {
            switch (event.keyCode) {
                case Keyboard.LEFT:
                    tetromino.moveLeft();
                    break;
                case Keyboard.RIGHT:
                    tetromino.moveRight();
                    break;
                case Keyboard.DOWN:
                    tetromino.moveDown();
                    break;
                case Keyboard.UP:
                    tetromino.rotateClockwise();
                    break;
            }
        }
    }
}
