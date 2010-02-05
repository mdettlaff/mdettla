package tetris {

    import flash.display.DisplayObject;
    import flash.events.Event;
    import flash.geom.Rectangle;

    import mx.core.UIComponent;
    import mx.managers.IFocusManagerComponent;

    public class GameCanvas
        extends UIComponent
        implements IFocusManagerComponent {

        public var bounds:Rectangle;
        public var lineColor:Number;
        public var fillColor:Number;

        public function GameCanvas(fillColor:Number = 0xFFFFFF,
                lineColor:Number = 0x000000) {
            super();
            var width:int = Board.WIDTH * Board.BLOCK_SIZE;
            var height:int = Board.HEIGHT * Board.BLOCK_SIZE;
            this.bounds = new Rectangle(0, 0, width, height);
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            drawBounds();
        }

        private function drawBounds():void {
            this.graphics.clear();
            this.graphics.lineStyle(1.0, this.lineColor, 1.0);
            this.graphics.beginFill(this.fillColor, 1.0);
            this.graphics.drawRect(
                    bounds.left - 1, bounds.top = 1,
                    bounds.width + 2, bounds.height + 2);
            this.graphics.endFill();
        }

        public function describeChildren(event:Event):void {
            var desc:String = "";
            var child:DisplayObject;
            for (var i:int=0; i < this.numChildren; i++) {
                child = this.getChildAt(i);
                desc += i + ": " + child + '\n';
            }
            trace(desc);
        }
    }
}
