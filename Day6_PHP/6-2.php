<?php
$frequencies = array();

function populate_frequencies_from_file(&$frequency_table, $filename) {
    $handle = fopen($filename, "r");

    while ($line = fgets($handle)) {
	$line_array = str_split($line);
	for ($i = 0; $i < sizeof($line_array); $i++) {
	    $c = $line_array[$i];

	    // initialize position in frequency array, if needed
	    if (!array_key_exists($i, $frequency_table))
		$frequency_table[$i] = array();

	    if (!array_key_exists($c, $frequency_table[$i]))
		$frequency_table[$i][$c] = 1;
	    else
		$frequency_table[$i][$c]++;
	}
    }
}

function lowest_frequency_in_pos($frequency_table, $n) {
    $min = PHP_INT_MAX;
    $return_char = ' ';

    foreach ($frequency_table[$n] as $char => $count) {
	if ($count < $min) {
	    $return_char = $char;
	    $min = $count;
	}
    }

    return $return_char;
}

function word_from_frequency_table($frequency_table) {
    $word = "";

    for ($i = 0; $i < sizeof($frequency_table); $i++) {
	$word .= lowest_frequency_in_pos($frequency_table, $i);
    }

    return $word;
}

populate_frequencies_from_file($frequencies, "input.txt");
echo word_from_frequency_table($frequencies);
