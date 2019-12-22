<template>
    <div id="main">
        <input type="button" value="⏏" @click="handleLoad">
        <input type="button" value="⏯" @click="handlePlayPause">
        <input type="button" value="⏹" @click="handleStop">
        <select name="select-port" id="select-port" @change="portSel" v-model="idx">
            <option v-bind:key="port.idx" v-for="port in ports" v-bind:value="port.idx">
                {{ port.name }}
            </option>
        </select>
    </div>
</template>

<script>
function submit(value) {
    window.external.invoke(value);
}

export default {
    name: 'app',
    data() {
        return {
            ports: [],
            idx: 0
        }
    },
    mounted() {
        window.portAdd = this.portAdd
    },
    methods: {
        portAdd(idx, name) {
            console.log("Added port with idx=" + idx + " and name=" + name);
            this.ports.push({idx: idx, name: name});
        },
        handleLoad() {
            submit('load')
        },
        handlePlayPause() {
            submit('playpause')
        },
        handleStop() {
            submit('stop')
        },
        portSel() {
            window.external.invoke("port" + this.idx);
        }
    }
}
</script>

<style scoped>
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
