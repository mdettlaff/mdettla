package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.geom.Rectangle;
    import flash.text.TextField;
    import flash.text.TextFormat;

    public class TypingArea extends Sprite {

        public static const MAX_LINES:int = 5;
        public static const LINE_HEIGHT:int = 44;
        public static const TEXT_TO_TYPE_COLOR:uint = 0x000000;
        public static const WRITTEN_TEXT_COLOR:uint = 0x0000C0;

        private var visibleTextLines:Array /* of String */;
        private var visibleWrittenLines:Array /* of String */;

        private var bounds:Rectangle;
        private var lineColor:uint;
        private var fillColor:uint;

        public function TypingArea(width:int, height:int,
                fillColor:uint = 0xFFFFFF, lineColor:uint = 0x000000) {
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            bounds = new Rectangle(0, 0, width, height);
            drawBounds();

            var i:int;
            var yShift:int;
            var line:TextField;

            // text to type
            visibleTextLines = [];
            yShift = 4;
            for (i = 0; i < MAX_LINES; i++) {
                line = createLine(yShift, TEXT_TO_TYPE_COLOR);
                visibleTextLines.push(line);
                addChild(line);
                yShift += LINE_HEIGHT;
            }

            // text typed in by the user
            visibleWrittenLines = [];
            yShift = 24;
            for (i = 0; i < MAX_LINES; i++) {
                line = createLine(yShift, WRITTEN_TEXT_COLOR);
                visibleWrittenLines.push(line);
                addChild(line);
                yShift += LINE_HEIGHT;
            }
        }

        public function draw(typingTestModel:TypingTestModel):void {
            var i:String;
            for (i in typingTestModel.textLines) {
                visibleTextLines[i].text = typingTestModel.textLines[i];
            }
            for (i in typingTestModel.writtenLines) {
                visibleWrittenLines[i].text = typingTestModel.writtenLines[i];
            }
        }

        private function createLine(yShift:int, color:uint):TextField {
            var format:TextFormat = new TextFormat();
            format.font = "Verdana";
            format.size = 15;
            format.color = color;

            var line:TextField = new TextField();
            line.x = 5;
            line.y = yShift;
            line.defaultTextFormat = format;
            line.text = "";
            line.selectable = false;
            line.width = bounds.width - 2;
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
