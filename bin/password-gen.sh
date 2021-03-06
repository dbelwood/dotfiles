#!/usr/bin/env bash

set -e

ADJECTIVES=(
    "adaptable"
    "adventurous"
    "affable"
    "affectionate"
    "agreeable"
    "ambitious"
    "amiable"
    "amicable"
    "amusing"
    "brave"
    "bright"
    "broad-minded"
    "calm"
    "careful"
    "charming"
    "communicative"
    "compassionate"
    "conscientious"
    "considerate"
    "convivial"
    "courageous"
    "courteous"
    "creative"
    "decisive"
    "determined"
    "diligent"
    "diplomatic"
    "discreet"
    "dynamic"
    "easygoing"
    "emotional"
    "energetic"
    "enthusiastic"
    "exuberant"
    "fair-minded"
    "faithful"
    "fearless"
    "forceful"
    "frank"
    "friendly"
    "funny"
    "generous"
    "gentle"
    "good"
    "gregarious"
    "hard-working"
    "helpful"
    "honest"
    "humorous"
    "imaginative"
    "impartial"
    "independent"
    "intellectual"
    "intelligent"
    "intuitive"
    "inventive"
    "kind"
    "loving"
    "loyal"
    "modest"
    "neat"
    "nice"
    "optimistic"
    "passionate"
    "patient"
    "persistent"
    "pioneering"
    "philosophical"
    "placid"
    "plucky"
    "polite"
    "powerful"
    "practical"
    "pro-active"
    "quick-witted"
    "quiet"
    "rational"
    "reliable"
    "reserved"
    "resourceful"
    "romantic"
    "self-confident"
    "self-disciplined"
    "sensible"
    "sensitive"
    "shy"
    "sincere"
    "sociable"
    "straightforward"
    "sympathetic"
    "thoughtful"
    "tidy"
    "tough"
    "unassuming"
    "understanding"
    "versatile"
    "warmhearted"
    "willing"
    "witty"
)

ANIMALS=(
    "ants"
    "bats"
    "bears"
    "bees"
    "birds"
    "buffalo"
    "buffalloes"
    "buffaloes"
    "cats"
    "chickens"
    "cattle"
    "dogs"
    "dolphins"
    "ducks"
    "elephants"
    "fish"
    "fishes"
    "foxes"
    "frogs"
    "geese"
    "goats"
    "horses"
    "kangaroos"
    "lions"
    "monkeys"
    "owls"
    "oxen"
    "penguins"
    "people"
    "pigs"
    "rabbits"
    "sheep"
    "tigers"
    "whales"
    "wolves"
    "zebras"
)

function capitalize() {
    echo "$(tr '[:lower:]' '[:upper:]' <<< ${1:0:1})${1:1}"
}

function random_number() {
    echo $(( $RANDOM % $1 ))
}

function random_item() {
    items=("$@")
    count=${#items[@]}
    index=$(random_number $count)
    echo "${items[$index]}"
}

echo "$(random_number 100)$(capitalize $(random_item "${ADJECTIVES[@]}"))$(capitalize $(random_item "${ANIMALS[@]}"))!"
