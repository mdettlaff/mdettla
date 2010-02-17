<?php

function ends_with($string, $substring) {
    $strlen = strlen($string);
    $substringlen = strlen($substring);
    if ($substringlen > $strlen) {
        return false;
    }
    return substr_compare($string, $substring, -$substringlen) === 0;
}

function list_text_files($dir_name) {
    $text_files = array();
    if ($handle = opendir($dir_name)) {
        while (false !== ($file = readdir($handle))) {
            if (ends_with($file, 'txt')) {
                $text_files[] = $file;
            }
        }
        closedir($handle);
    }
    return $text_files;
}

function file_contents($file_path) {
    $text = '';
    $file = fopen($file_path, "r") or exit('ERROR: Unable to open file.');
    while (!feof($file)) {
        $text .= fgets($file);
    }
    fclose($file);
    return $text;
}

$TEXTS_DIR = 'texts';

$action = $_GET['action'];
$text_index = $_GET['text_index'];

if ($action == 'get_text' && isset($text_index)) {
    $text_files = list_text_files($TEXTS_DIR);
    if ($text_index < 0 || $text_index > count($text_files) - 1) {
        exit('ERROR: parameter text_index must be between 0 and '
            . (count($text_files) - 1) . " ($text_index given)");
    }
    $text_file_path = $TEXTS_DIR . '/' . $text_files[$text_index];
    echo file_contents($text_file_path);
} else if ($action == 'get_texts_count') {
    $text_files = list_text_files($TEXTS_DIR);
    echo count($text_files);
} else {
    echo 'ERROR: Missing parameters.';
}

?>
