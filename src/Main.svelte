<script>
    let ports = [];
    let curr;

    const submit = window.external.invoke;

    const handleLoad = () => submit('load');
    const handlePlayPause = () => submit('playpause');
    const handleStop = () => submit('stop');

    function portAdd(idx, name) {
        ports = [...ports, {idx: idx, name: name}];
    }

    const portSel = () => window.external.invoke("port" + curr);

    // Haskell export
    window.portAdd = portAdd;
</script>

<style>
    #main {
        display: grid;
        width: 100%;
        height: 100%;
        grid-template-columns: 172px 172px 172px;
        grid-template-rows: 172px 50px;
        grid-column-gap: 9px;
        grid-row-gap: 9px;
    }
    input[type=button] {
        font-size: 10vw;
        width: 100%;
        height: 100%;
    }
    #select-port {
        grid-column-start: 1;
        grid-column-end: 4;
    }
</style>

<div id="main">
    <input type="button" value="⏏" on:click={handleLoad} >
    <input type="button" value="⏯" on:click={handlePlayPause} >
    <input type="button" value="⏹" on:click={handleStop} >
    <select name="select-port" id="select-port" bind:value={curr}
            on:change={portSel} >
        {#each ports as port (port.idx)}
            <option value={port.idx}>
                {port.name}
            </option>
        {/each}
    </select>
</div>