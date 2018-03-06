#!/bin/bash

idris --build js.ipkg &&
idris --mkdoc js.ipkg
