import React from 'react';
import ReactDOM from 'react-dom';
import './Main.css';

const submit = window.external.invoke;

function portAdd(idx, name) {
    var select = document.getElementById("select-port");
    var option = document.createElement("option");
    option.value = idx;
    option.innerText = name;
    select.appendChild(option);
}

const portSel = () => {
    var select = document.getElementById("select-port");
    var idx = select.value;
    console.log("port"+idx);
    window.external.invoke("port" + idx);
}

const Button = ({icon, cmd}) =>
    <input type="button" value={icon} onClick={() => submit(cmd)} />

// TODO: populate with options dynamically
const Select = () =>
    <select name="select-port" id="select-port"
        className="select-port" onChange={portSel} >
    </select>

const App = () =>
    <div id="main" className="main">
        <Button icon="⏏" cmd='load' />
        <Button icon="⏯" cmd='playpause' />
        <Button icon="⏹" cmd='stop' />
        <Select />
    </div>;

ReactDOM.render(<App />, document.getElementById('app'));

// Haskell exports
window.portAdd = portAdd;
