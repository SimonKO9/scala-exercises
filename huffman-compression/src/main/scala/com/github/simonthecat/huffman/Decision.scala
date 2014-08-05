package com.github.simonthecat.huffman

sealed trait Decision

case object Left extends Decision

case object Right extends Decision
