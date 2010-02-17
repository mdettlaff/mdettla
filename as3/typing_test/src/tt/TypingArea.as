package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.geom.Rectangle;
    import flash.text.TextField;
    import flash.text.TextFormat;

    public class TypingArea extends Sprite {

        public static const MAX_LINES:int = 5;

        private var visibleTextLines:Array /* of String */;

        private var bounds:Rectangle;
        private var lineColor:uint;
        private var fillColor:uint;

        public function TypingArea(width:int, height:int,
                fillColor:uint = 0xFFFFFF, lineColor:uint = 0x000000) {
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            bounds = new Rectangle(0, 0, width, height);
            drawBounds();

            visibleTextLines = [];
            var yShift:int = 4;
            for (var i:int = 0; i < MAX_LINES; i++) {
                var line:TextField = createLine(yShift);
                visibleTextLines.push(line);
                addChild(line);
                yShift += 40;
            }
        }

        public function draw(typingTestModel:TypingTestModel):void {
            for (var i:String in typingTestModel.textLines) {
                visibleTextLines[i].text = typingTestModel.textLines[i];
            }
        }

        private function createLine(yShift:int):TextField {
            var format:TextFormat = new TextFormat();
            format.font = "Verdana";
            format.size = 16;

            var line:TextField = new TextField();
            line.x = 5;
            line.y = yShift;
            line.defaultTextFormat = format;
            line.text = "początek linii";
            line.selectable = false;
            line.width = bounds.width;
            return line;
        }

        private function drawBounds():void {
            graphics.clear();
            graphics.lineStyle(1.0, this.lineColor);
            graphics.beginFill(this.fillColor);
            graphics.drawRect(
                    bounds.left, bounds.top,
                    bounds.width, bounds.height);
            graphics.endFill();
        }
    }
}
