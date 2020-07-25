var countUniqueSirenAndColorThem = () => {
    sirenMap = sirenLiToMap()
    uniqueCount = duplicatedCount = 0
    liList = ""
    for (const [siren, duplicated] of sirenMap) {
        if (duplicated) duplicatedCount += 1
        else uniqueCount += 1
        color = duplicated ? 'blue':'green'
        liList += `<li style="color:${color}">${siren.toString().padStart(9, '0')}</li>`
    }
    displayStats(uniqueCount, duplicatedCount)
    displaySirenLi(liList)    
    return [uniqueCount, duplicatedCount]
}

var displayStats = (uniqueCount, duplicatedCount) => {
    $('#stats').empty()    
    $('#stats').append(
        `<p>The list contains:</p>
         <p>${uniqueCount} unique sirens (in green)</p>
         <p>${duplicatedCount} duplicated sirens (in blue)</p>`)
}

var displaySirenLi = async liList => {
    setTimeout(() => {
        $('ul').empty()
        $('ul').append(liList)
    }, 0)
}

var sirenLiToMap = () => {
    const regex = new RegExp("^[0-9]{9}$")
    sirenMap = new Map()
    $('li').each(function(i) {
        if (regex.test($(this).text())) {
            siren = sirenFromInt(parseInt($(this).text()))
            if (siren) {
                if (!sirenMap.has(siren)) sirenMap.set(siren, false)
                else sirenMap.set(siren, true)
            }
        }
    })
    return sirenMap
}     

var sirenFromInt = x => 
    0 <= x && x < 10^9 && isLuhn(x) ? x : null

var isLuhn = x =>
    toLuhnForm(x).reduce((a, b) => a + b) % 10 == 0

var toLuhnForm = x => {
    var sumDigit = z => z > 9 ? z-9 : z
    digits = revDigits(x)
    ans = []
    for (i = 0; i<digits.length-1; i+=2)
        ans.push(digits[i], sumDigit(digits[i+1]*2))
    if(ans.length < digits.length)
        ans.push(digits[i])
    return ans.reverse()
}

var revDigits = x => {
    if (x == 0) return [0]
    ans = []
    while(x!=0) {
        [x, r] = divMod(x, 10)
        ans.push(r)
    }   
    return ans
}

var divMod = (a, b) =>
    [~~(a/b), a%b]