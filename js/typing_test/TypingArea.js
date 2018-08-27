class TypingArea {

    constructor(context, width, height) {
        this.MAX_LINES = 12;
        this.MAX_VISIBLE_WRITTEN_LINES = 3;
        this.LINE_HEIGHT = 22;
        this.TOP_MARGIN_TEXT = 22;
        this.TOP_MARGIN_WRITTEN = 42;
        this.TEXT_TO_TYPE_COLOR = '#000000';
        this.WRITTEN_TEXT_COLOR = '#0000C0';
        this.MISTAKE_TEXT_COLOR = '#F00000';
        this.CORRECTED_TEXT_COLOR = '#C000D8';
        this.REGULAR_FONT = '15px Verdana';
        this.BOLD_FONT = 'bold ' + this.REGULAR_FONT;

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
                    - this.MAX_VISIBLE_WRITTEN_LINES,
                0);
        const endLine = Math.min(
                startLine + this.MAX_VISIBLE_WRITTEN_LINES - 1,
                typingTestModel.writtenLines.length - 1);

        this.context.font = this.REGULAR_FONT;
        if (typingTestModel.isReady) {
            this.drawWrittenLines(typingTestModel, startLine, endLine, drawOnlyCurrentLine);
        }
        if (!drawOnlyCurrentLine) {
            this.drawTextLines(typingTestModel.textLines, startLine, endLine);
        }
    }

    createVisibleTextLines() {
        var visibleTextLines = [];
        for (var i = 0; i < this.MAX_LINES; i++) {
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
                && i < endLine + 1 + this.MAX_LINES
                    - 2 * (1 + endLine - startLine); i++) {
            this.visibleTextLines[visibleIndex] = textLines[i];
            visibleIndex += 1;
        }
        while (visibleIndex < this.visibleTextLines.length) {
            this.visibleTextLines[visibleIndex] = "";
            visibleIndex += 1;
        }

        this.context.fillStyle = this.TEXT_TO_TYPE_COLOR;
        var yShift = this.TOP_MARGIN_TEXT;
        for (var i = 0; i < this.MAX_LINES; i++) {
            this.context.fillText(this.visibleTextLines[i], 10, yShift);
            yShift += this.LINE_HEIGHT;
        }
    }

    drawWrittenLines(typingTestModel,
            startLine, endLine, drawOnlyCurrentLine) {
        this.context.fillStyle = this.WRITTEN_TEXT_COLOR;
        var yShift = this.TOP_MARGIN_WRITTEN;
        var visibleIndex = 0;
        var x = 10;
        for (var i = startLine; i <= endLine; i++) {
            if (!drawOnlyCurrentLine || i == endLine) {
                x = 10;
                for (var j = 0; j < typingTestModel.writtenLines[i].length; j++) {
                    var c = typingTestModel.writtenLines[i].charAt(j);
                    this.drawWrittenCharacter(c, x, yShift, i, j, typingTestModel);
                    x += this.context.measureText(c).width;
                }
            }
            visibleIndex += 1;
            yShift += 2 * this.LINE_HEIGHT;
        }
        yShift -= 2 * this.LINE_HEIGHT;
        const cursor = '_';
        this.context.fillStyle = typingTestModel.isMistakeMade ? this.MISTAKE_TEXT_COLOR : this.WRITTEN_TEXT_COLOR;
        this.context.fillText(cursor, x, yShift);
    }

    drawWrittenCharacter(c, x, yShift, i, j, typingTestModel) {
        if (typingTestModel.mistakes[i][j]) {
            this.context.fillStyle = this.MISTAKE_TEXT_COLOR;
            this.context.font = this.BOLD_FONT;
        } else if (typingTestModel.corrections[i][j]) {
            this.context.fillStyle = this.CORRECTED_TEXT_COLOR;
        } else {
            this.context.fillStyle = this.WRITTEN_TEXT_COLOR;
        }
        this.context.fillText(c, x, yShift);
        if (typingTestModel.mistakes[i][j]) {
            this.context.font = this.REGULAR_FONT;
        }
    }
}
