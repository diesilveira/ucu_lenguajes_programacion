
function distance(p1, p2) {
    if (!p2) return NaN
    return ((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2) ** 0.5
}

// console.log(distance({x:2, y:6}, undefined))

function* range(x, y, z = 1) {
    for (; x < y; x += z) {
        yield x
    }
}

// console.log([...range(0,10,2)])

async function delay(time, res = Date.now()) {
    return await new Promise(resolve =>
        setTimeout(() => {
            resolve(res);
        }, time)
    )
}

async function asyncTryMany(values, fun) {
    const promises = values.map(fun)
    return (await Promise.allSettled(promises))
        .filter(({ status }) => status === 'fulfilled')
        .map(({ value }) => value)
}

async function asyncGoUntil(values, fun, time) {
    let results = []
    let completed = false
    let timer = new Promise(resolve =>
        setTimeout(() => {
            completed = true
            resolve()
        }, time)
    )

    for (const value of values) {
        // Race current Promise against timer
        let result = await Promise.race([fun(value), timer])
        if (completed) break
        // Store if timer is not completed
        results.push(result)
    }

    return results
}
