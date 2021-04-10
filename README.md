
<p align="center">
  <a href="https://github.com/mileswatson/qupl/blob/master/LICENSE"><img alt="GitHub license" src="https://img.shields.io/github/license/mileswatson/qupl?color=blue"></a>
  <a href="https://github.com/mileswatson/qupl/stargazers"><img alt="GitHub stars" src="https://img.shields.io/github/stars/mileswatson/qupl?color=gold"></a>
  <a href="https://github.com/mileswatson/Hosta/issues"><img alt="GitHub issues" src="https://img.shields.io/github/issues/mileswatson/Hosta"></a>
  <img alt=".NET 5" src="https://img.shields.io/static/v1?label=&message=%2ENET%205&color=5C2D91">
  <img alt="Windows" src="https://img.shields.io/static/v1?label=&message=Windows&color=0078D6&logo=Windows">
  <img alt="Linux" src="https://img.shields.io/static/v1?label=&message=linux&color=FCC624&logo=Linux&logoColor=black">
</p>

<br>

<p align="center">

  <h3 align="center">
    <a href="#----------------">
      <img src="img/logo.png" alt="Logo" width="400" height="166">
    </a>
  </h3>

  <h3 align="center">Quantum computing made simple.</h3>

  <br>

  <p align="center">
    A simple, interpreted <b>Qu</b>antum <b>P</b>rogramming <b>L</b>anguage.
    <br />
    <a href="https://github.com/mileswatson/qupl"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/mileswatson/qupl">View Demo</a>
    ·
    <a href="https://github.com/mileswatson/qupl/issues">Report Bug</a>
    ·
    <a href="https://github.com/mileswatson/qupl/issues">Request Feature</a>
  </p>
</p>

<h1></h1>

### Sample Program (Deutsch's Algorithm)

Deutsch's algorithm can be used to tell in a single pass whether a function is constant or balanced.

```fs

// A function is sequence of parallel gates

funq blackbox =
    I I         // These are both identity gates - they do nothing
    CNOT        // This gate is a CNOT - it is two qubits wide

// A let definition is starting state, which can
// be followed by a sequence of parallel gates.
let deutsch =
    0 1         // These are the initial starting states
    H H

    log         // You can debug intermediate states in a 'let' expression
    blackbox
    log

    H H

let main =
    deutsch

```