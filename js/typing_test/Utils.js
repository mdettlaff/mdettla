    class Utils {

        constructor() {
            this.PL_TO_EN = {
                'ą': 'a', 'ć': 'c', 'ę': 'e', 'ł': 'l', 'ń': 'n', 'ó': 'o',
                'ś': 's', 'ż': 'z', 'ź': 'z', 'Ą': 'A', 'Ć': 'C', 'Ę': 'E',
                'Ł': 'L', 'Ń': 'N', 'Ó': 'O', 'Ś': 'S', 'Ż': 'Z', 'Ź': 'Z'
            };
        }

        breakLines(text, maxLineLength) /* of String */ {
            const multiSpace = / +/g;
            var textLines = text.replace(multiSpace, ' ').trim().split('\n');
            var lineEndIndex = 0;
            for (var i = 0; i < textLines.length; i++) {
                for (var j = 1; j < textLines[i].length
                        && j <= maxLineLength; j++) {
                    if (textLines[i].charAt(j) == ' '
                            || textLines[i].charAt(j) == '\t') {
                        lineEndIndex = j;
                    }
                    if (j == maxLineLength) { // break line
                        textLines.splice(i + 1, 0,
                                textLines[i].substring(lineEndIndex + 1));
                        textLines[i] = textLines[i].substring(0, lineEndIndex);
                    }
                }
            }
            return textLines;
        }

        shavePlChars(withPlChars) {
            var withoutPlChars = "";
            for (var i = 0; i < withPlChars.length; i++) {
                var c = withPlChars.charAt(i);
                withoutPlChars += c in this.PL_TO_EN ? this.PL_TO_EN[c] : c;
            }
            return withoutPlChars;
        }

        containsPlChars(s) {
            for (var i = 0; i < s.length; i++) {
                if (s.charAt(i) in this.PL_TO_EN) {
                    return true;
                }
            }
            return false;
        }
    }

