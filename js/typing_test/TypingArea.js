const MAX_LINES = 12;
const MAX_VISIBLE_WRITTEN_LINES = 3;
const LINE_HEIGHT = 22;
const TOP_MARGIN_TEXT = 22;
const TOP_MARGIN_WRITTEN = 42;
const TEXT_TO_TYPE_COLOR = '#000000';
const WRITTEN_TEXT_COLOR = '#0000C0';
const MISTAKE_TEXT_COLOR = '#F00000';
const CORRECTED_TEXT_COLOR = '#C000D8';
const REGULAR_FONT = '15px Verdana';
const BOLD_FONT = 'bold ' + REGULAR_FONT;

class TypingArea {

    constructor(context, width, height) {
        this.context = context;
        this.width = width;
        this.height = height;
        // text to type, not written by the user
        this.visibleTextLines = this.createVisibleTextLines();
    }

    draw(typingTestModel) {
        // TODO do we need the drawOnlyCurrentLine variable?
        const drawOnlyCurrentLine = typingTestModel.stayedInTheSameLine && false;
        const startLine = Math.max(
                typingTestModel.writtenLines.length
                    - MAX_VISIBLE_WRITTEN_LINES,
                0);
        const endLine = Math.min(
                startLine + MAX_VISIBLE_WRITTEN_LINES - 1,
                typingTestModel.writtenLines.length - 1);

        this.context.font = REGULAR_FONT;
        if (typingTestModel.isReady) {
            this.drawWrittenLines(typingTestModel, startLine, endLine, drawOnlyCurrentLine);
        }
        if (!drawOnlyCurrentLine) {
            this.drawTextLines(typingTestModel.textLines, startLine, endLine);
        }
    }

    createVisibleTextLines() {
        var visibleTextLines = [];
        for (var i = 0; i < MAX_LINES; i++) {
            var line = "";
            visibleTextLines.push(line);
        }
        return visibleTextLines;
    }

    drawTextLines(textLines,
            startLine, endLine) {
        var visibleIndex = 0;
        var i;
        for (i = startLine; i <= endLine; i++) {
            this.visibleTextLines[visibleIndex] = textLines[i];
            visibleIndex += 1;
            this.visibleTextLines[visibleIndex] = "";
            visibleIndex += 1;
        }
        for (i = endLine + 1; i < textLines.length
                && i < endLine + 1 + MAX_LINES
                    - 2 * (1 + endLine - startLine); i++) {
            this.visibleTextLines[visibleIndex] = textLines[i];
            visibleIndex += 1;
        }
        while (visibleIndex < this.visibleTextLines.length) {
            this.visibleTextLines[visibleIndex] = "";
            visibleIndex += 1;
        }

        context.fillStyle = TEXT_TO_TYPE_COLOR;
        var yShift = TOP_MARGIN_TEXT;
        for (var i = 0; i < MAX_LINES; i++) {
            context.fillText(this.visibleTextLines[i], 10, yShift);
            yShift += LINE_HEIGHT;
        }
    }

    drawWrittenLines(typingTestModel,
            startLine, endLine, drawOnlyCurrentLine) {
        context.fillStyle = WRITTEN_TEXT_COLOR;
        var yShift = TOP_MARGIN_WRITTEN;
        var visibleIndex = 0;
        var x = 10;
        for (var i = startLine; i <= endLine; i++) {
            if (!drawOnlyCurrentLine || i == endLine) {
                x = 10;
                for (var j = 0; j < typingTestModel.writtenLines[i].length; j++) {
                    var c = typingTestModel.writtenLines[i].charAt(j);
                    this.drawWrittenCharacter(c, x, yShift, i, j, typingTestModel);
                    x += context.measureText(c).width;
                }
            }
            visibleIndex += 1;
            yShift += 2 * LINE_HEIGHT;
        }
        yShift -= 2 * LINE_HEIGHT;
        const cursor = '_';
        context.fillStyle = model.isMistakeMade ? MISTAKE_TEXT_COLOR : WRITTEN_TEXT_COLOR;
        context.fillText(cursor, x, yShift);
    }

    drawWrittenCharacter(c, x, yShift, i, j, model) {
        if (model.mistakes[i][j]) {
            this.context.fillStyle = MISTAKE_TEXT_COLOR;
            this.context.font = BOLD_FONT;
        } else if (model.corrections[i][j]) {
            this.context.fillStyle = CORRECTED_TEXT_COLOR;
        } else {
            this.context.fillStyle = WRITTEN_TEXT_COLOR;
        }
        this.context.fillText(c, x, yShift);
        if (model.mistakes[i][j]) {
            this.context.font = REGULAR_FONT;
        }
    }
}
