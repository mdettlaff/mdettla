#!/bin/bash

echo "Content-type: text/html"
echo ""
echo "<HTML>"
echo "<BODY>"
echo "Podano napis: $QUERY_STRING"
echo "</BODY>"
echo "</HTML>"
exit 0
