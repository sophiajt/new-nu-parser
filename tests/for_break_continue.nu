mut x = 0
for i in [1 2 3] {
    if $x > 2 {
        break
    }

    if $i < 3 {
        continue
    }

    $x = $x + $i
}