module.exports =
/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 682);
/******/ })
/************************************************************************/
/******/ ({

/***/ 12:
/***/ (function(module, exports, __webpack_require__) {

"use strict";
/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 * Modification copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/

var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.expandFilePathInOutput = exports.getToolFromToolPath = exports.fixDriveCasingInWindows = exports.getCurrentGoWorkspaceFromGOPATH = exports.getInferredGopath = exports.resolveHomeDir = exports.clearCacheForTools = exports.pathExists = exports.fileExists = exports.executableFileExists = exports.correctBinname = exports.setCurrentGoRoot = exports.getCurrentGoRoot = exports.getBinPathWithPreferredGopathGorootWithExplanation = exports.getBinPathWithPreferredGopathGoroot = exports.getBinPathFromEnvVar = exports.envPath = void 0;
/**
 * This file is loaded by both the extension and debug adapter, so it cannot import 'vscode'
 */
const fs = __webpack_require__(6);
const os = __webpack_require__(29);
const path = __webpack_require__(2);
const util_1 = __webpack_require__(3);
const goLogging_1 = __webpack_require__(42);
let binPathCache = {};
exports.envPath = process.env['PATH'] || (process.platform === 'win32' ? process.env['Path'] : null);
// find the tool's path from the given PATH env var, or null if the tool is not found.
function getBinPathFromEnvVar(toolName, envVarValue, appendBinToPath) {
    toolName = correctBinname(toolName);
    if (envVarValue) {
        const paths = envVarValue.split(path.delimiter);
        for (const p of paths) {
            const binpath = path.join(p, appendBinToPath ? 'bin' : '', toolName);
            if (executableFileExists(binpath)) {
                return binpath;
            }
        }
    }
    return null;
}
exports.getBinPathFromEnvVar = getBinPathFromEnvVar;
function getBinPathWithPreferredGopathGoroot(toolName, preferredGopaths, preferredGoroot, alternateTool, useCache = true) {
    const r = getBinPathWithPreferredGopathGorootWithExplanation(toolName, preferredGopaths, preferredGoroot, alternateTool, useCache);
    return r.binPath;
}
exports.getBinPathWithPreferredGopathGoroot = getBinPathWithPreferredGopathGoroot;
// Is same as getBinPathWithPreferredGopathGoroot, but returns why the
// returned path was chosen.
function getBinPathWithPreferredGopathGorootWithExplanation(toolName, preferredGopaths, preferredGoroot, alternateTool, useCache = true) {
    if (alternateTool && path.isAbsolute(alternateTool) && executableFileExists(alternateTool)) {
        binPathCache[toolName] = alternateTool;
        return { binPath: alternateTool, why: 'alternateTool' };
    }
    // FIXIT: this cache needs to be invalidated when go.goroot or go.alternateTool is changed.
    if (useCache && binPathCache[toolName]) {
        return { binPath: binPathCache[toolName], why: 'cached' };
    }
    const binname = alternateTool && !path.isAbsolute(alternateTool) ? alternateTool : toolName;
    const found = (why) => binname === toolName ? why : 'alternateTool';
    const pathFromGoBin = getBinPathFromEnvVar(binname, process.env['GOBIN'], false);
    if (pathFromGoBin) {
        binPathCache[toolName] = pathFromGoBin;
        return { binPath: pathFromGoBin, why: binname === toolName ? 'gobin' : 'alternateTool' };
    }
    for (const preferred of preferredGopaths) {
        if (typeof preferred === 'string') {
            // Search in the preferred GOPATH workspace's bin folder
            const pathFrompreferredGoPath = getBinPathFromEnvVar(binname, preferred, true);
            if (pathFrompreferredGoPath) {
                binPathCache[toolName] = pathFrompreferredGoPath;
                return { binPath: pathFrompreferredGoPath, why: found('gopath') };
            }
        }
    }
    // Check GOROOT (go, gofmt, godoc would be found here)
    const pathFromGoRoot = getBinPathFromEnvVar(binname, preferredGoroot || getCurrentGoRoot(), true);
    if (pathFromGoRoot) {
        binPathCache[toolName] = pathFromGoRoot;
        return { binPath: pathFromGoRoot, why: found('goroot') };
    }
    // Finally search PATH parts
    const pathFromPath = getBinPathFromEnvVar(binname, exports.envPath, false);
    if (pathFromPath) {
        binPathCache[toolName] = pathFromPath;
        return { binPath: pathFromPath, why: found('path') };
    }
    // Check common paths for go
    if (toolName === 'go') {
        const defaultPathsForGo = process.platform === 'win32' ? ['C:\\Go\\bin\\go.exe'] : ['/usr/local/go/bin/go', '/usr/local/bin/go'];
        for (const p of defaultPathsForGo) {
            if (executableFileExists(p)) {
                binPathCache[toolName] = p;
                return { binPath: p, why: 'default' };
            }
        }
        return { binPath: '' };
    }
    // Else return the binary name directly (this will likely always fail downstream)
    return { binPath: toolName };
}
exports.getBinPathWithPreferredGopathGorootWithExplanation = getBinPathWithPreferredGopathGorootWithExplanation;
/**
 * Returns the goroot path if it exists, otherwise returns an empty string
 */
let currentGoRoot = '';
function getCurrentGoRoot() {
    return currentGoRoot || process.env['GOROOT'] || '';
}
exports.getCurrentGoRoot = getCurrentGoRoot;
function setCurrentGoRoot(goroot) {
    goLogging_1.logVerbose(`setCurrentGoRoot(${goroot})`);
    currentGoRoot = goroot;
}
exports.setCurrentGoRoot = setCurrentGoRoot;
function correctBinname(toolName) {
    if (process.platform === 'win32') {
        return toolName + '.exe';
    }
    return toolName;
}
exports.correctBinname = correctBinname;
function executableFileExists(filePath) {
    let exists = true;
    try {
        exists = fs.statSync(filePath).isFile();
        if (exists) {
            fs.accessSync(filePath, fs.constants.F_OK | fs.constants.X_OK);
        }
    }
    catch (e) {
        exists = false;
    }
    return exists;
}
exports.executableFileExists = executableFileExists;
function fileExists(filePath) {
    try {
        return fs.statSync(filePath).isFile();
    }
    catch (e) {
        return false;
    }
}
exports.fileExists = fileExists;
function pathExists(p) {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            const stat = util_1.promisify(fs.stat);
            return (yield stat(p)).isDirectory();
        }
        catch (e) {
            return false;
        }
    });
}
exports.pathExists = pathExists;
function clearCacheForTools() {
    binPathCache = {};
}
exports.clearCacheForTools = clearCacheForTools;
/**
 * Exapnds ~ to homedir in non-Windows platform
 */
function resolveHomeDir(inputPath) {
    if (!inputPath || !inputPath.trim()) {
        return inputPath;
    }
    return inputPath.startsWith('~') ? path.join(os.homedir(), inputPath.substr(1)) : inputPath;
}
exports.resolveHomeDir = resolveHomeDir;
// Walks up given folder path to return the closest ancestor that has `src` as a child
function getInferredGopath(folderPath) {
    if (!folderPath) {
        return;
    }
    const dirs = folderPath.toLowerCase().split(path.sep);
    // find src directory closest to given folder path
    const srcIdx = dirs.lastIndexOf('src');
    if (srcIdx > 0) {
        return folderPath.substr(0, dirs.slice(0, srcIdx).join(path.sep).length);
    }
}
exports.getInferredGopath = getInferredGopath;
/**
 * Returns the workspace in the given Gopath to which given directory path belongs to
 * @param gopath string Current Gopath. Can be ; or : separated (as per os) to support multiple paths
 * @param currentFileDirPath string
 */
function getCurrentGoWorkspaceFromGOPATH(gopath, currentFileDirPath) {
    if (!gopath) {
        return;
    }
    const workspaces = gopath.split(path.delimiter);
    let currentWorkspace = '';
    currentFileDirPath = fixDriveCasingInWindows(currentFileDirPath);
    // Find current workspace by checking if current file is
    // under any of the workspaces in $GOPATH
    for (const workspace of workspaces) {
        const possibleCurrentWorkspace = path.join(workspace, 'src');
        if (currentFileDirPath.startsWith(possibleCurrentWorkspace) ||
            (process.platform === 'win32' &&
                currentFileDirPath.toLowerCase().startsWith(possibleCurrentWorkspace.toLowerCase()))) {
            // In case of nested workspaces, (example: both /Users/me and /Users/me/src/a/b/c are in $GOPATH)
            // both parent & child workspace in the nested workspaces pair can make it inside the above if block
            // Therefore, the below check will take longer (more specific to current file) of the two
            if (possibleCurrentWorkspace.length > currentWorkspace.length) {
                currentWorkspace = currentFileDirPath.substr(0, possibleCurrentWorkspace.length);
            }
        }
    }
    return currentWorkspace;
}
exports.getCurrentGoWorkspaceFromGOPATH = getCurrentGoWorkspaceFromGOPATH;
// Workaround for issue in https://github.com/Microsoft/vscode/issues/9448#issuecomment-244804026
function fixDriveCasingInWindows(pathToFix) {
    return process.platform === 'win32' && pathToFix
        ? pathToFix.substr(0, 1).toUpperCase() + pathToFix.substr(1)
        : pathToFix;
}
exports.fixDriveCasingInWindows = fixDriveCasingInWindows;
/**
 * Returns the tool name from the given path to the tool
 * @param toolPath
 */
function getToolFromToolPath(toolPath) {
    if (!toolPath) {
        return;
    }
    let tool = path.basename(toolPath);
    if (process.platform === 'win32' && tool.endsWith('.exe')) {
        tool = tool.substr(0, tool.length - 4);
    }
    return tool;
}
exports.getToolFromToolPath = getToolFromToolPath;
/**
 * Returns output with relative filepaths expanded using the provided directory
 * @param output
 * @param cwd
 */
function expandFilePathInOutput(output, cwd) {
    const lines = output.split('\n');
    for (let i = 0; i < lines.length; i++) {
        const matches = lines[i].match(/\s*(\S+\.go):(\d+):/);
        if (matches && matches[1] && !path.isAbsolute(matches[1])) {
            lines[i] = lines[i].replace(matches[1], path.join(cwd, matches[1]));
        }
    }
    return lines.join('\n');
}
exports.expandFilePathInOutput = expandFilePathInOutput;


/***/ }),

/***/ 132:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.ProtocolServer = void 0;
const ee = __webpack_require__(33);
const messages_1 = __webpack_require__(47);
class Disposable0 {
    dispose() {
    }
}
class Emitter {
    get event() {
        if (!this._event) {
            this._event = (listener, thisArg) => {
                this._listener = listener;
                this._this = thisArg;
                let result;
                result = {
                    dispose: () => {
                        this._listener = undefined;
                        this._this = undefined;
                    }
                };
                return result;
            };
        }
        return this._event;
    }
    fire(event) {
        if (this._listener) {
            try {
                this._listener.call(this._this, event);
            }
            catch (e) {
            }
        }
    }
    hasListener() {
        return !!this._listener;
    }
    dispose() {
        this._listener = undefined;
        this._this = undefined;
    }
}
class ProtocolServer extends ee.EventEmitter {
    constructor() {
        super();
        this._sendMessage = new Emitter();
        this._pendingRequests = new Map();
        this.onDidSendMessage = this._sendMessage.event;
    }
    // ---- implements vscode.Debugadapter interface ---------------------------
    dispose() {
    }
    handleMessage(msg) {
        if (msg.type === 'request') {
            this.dispatchRequest(msg);
        }
        else if (msg.type === 'response') {
            const response = msg;
            const clb = this._pendingRequests.get(response.request_seq);
            if (clb) {
                this._pendingRequests.delete(response.request_seq);
                clb(response);
            }
        }
    }
    _isRunningInline() {
        return this._sendMessage && this._sendMessage.hasListener();
    }
    //--------------------------------------------------------------------------
    start(inStream, outStream) {
        this._sequence = 1;
        this._writableStream = outStream;
        this._rawData = new Buffer(0);
        inStream.on('data', (data) => this._handleData(data));
        inStream.on('close', () => {
            this._emitEvent(new messages_1.Event('close'));
        });
        inStream.on('error', (error) => {
            this._emitEvent(new messages_1.Event('error', 'inStream error: ' + (error && error.message)));
        });
        outStream.on('error', (error) => {
            this._emitEvent(new messages_1.Event('error', 'outStream error: ' + (error && error.message)));
        });
        inStream.resume();
    }
    stop() {
        if (this._writableStream) {
            this._writableStream.end();
        }
    }
    sendEvent(event) {
        this._send('event', event);
    }
    sendResponse(response) {
        if (response.seq > 0) {
            console.error(`attempt to send more than one response for command ${response.command}`);
        }
        else {
            this._send('response', response);
        }
    }
    sendRequest(command, args, timeout, cb) {
        const request = {
            command: command
        };
        if (args && Object.keys(args).length > 0) {
            request.arguments = args;
        }
        this._send('request', request);
        if (cb) {
            this._pendingRequests.set(request.seq, cb);
            const timer = setTimeout(() => {
                clearTimeout(timer);
                const clb = this._pendingRequests.get(request.seq);
                if (clb) {
                    this._pendingRequests.delete(request.seq);
                    clb(new messages_1.Response(request, 'timeout'));
                }
            }, timeout);
        }
    }
    // ---- protected ----------------------------------------------------------
    dispatchRequest(request) {
    }
    // ---- private ------------------------------------------------------------
    _emitEvent(event) {
        this.emit(event.event, event);
    }
    _send(typ, message) {
        message.type = typ;
        message.seq = this._sequence++;
        if (this._writableStream) {
            const json = JSON.stringify(message);
            this._writableStream.write(`Content-Length: ${Buffer.byteLength(json, 'utf8')}\r\n\r\n${json}`, 'utf8');
        }
        this._sendMessage.fire(message);
    }
    _handleData(data) {
        this._rawData = Buffer.concat([this._rawData, data]);
        while (true) {
            if (this._contentLength >= 0) {
                if (this._rawData.length >= this._contentLength) {
                    const message = this._rawData.toString('utf8', 0, this._contentLength);
                    this._rawData = this._rawData.slice(this._contentLength);
                    this._contentLength = -1;
                    if (message.length > 0) {
                        try {
                            let msg = JSON.parse(message);
                            this.handleMessage(msg);
                        }
                        catch (e) {
                            this._emitEvent(new messages_1.Event('error', 'Error handling data: ' + (e && e.message)));
                        }
                    }
                    continue; // there may be more complete messages to process
                }
            }
            else {
                const idx = this._rawData.indexOf(ProtocolServer.TWO_CRLF);
                if (idx !== -1) {
                    const header = this._rawData.toString('utf8', 0, idx);
                    const lines = header.split('\r\n');
                    for (let i = 0; i < lines.length; i++) {
                        const pair = lines[i].split(/: +/);
                        if (pair[0] == 'Content-Length') {
                            this._contentLength = +pair[1];
                        }
                    }
                    this._rawData = this._rawData.slice(idx + ProtocolServer.TWO_CRLF.length);
                    continue;
                }
            }
            break;
        }
    }
}
exports.ProtocolServer = ProtocolServer;
ProtocolServer.TWO_CRLF = '\r\n\r\n';
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicHJvdG9jb2wuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9zcmMvcHJvdG9jb2wudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Z0dBR2dHOzs7QUFFaEcsNkJBQTZCO0FBRTdCLHlDQUE2QztBQVM3QyxNQUFNLFdBQVc7SUFDaEIsT0FBTztJQUNQLENBQUM7Q0FDRDtBQU1ELE1BQU0sT0FBTztJQU1aLElBQUksS0FBSztRQUNSLElBQUksQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFO1lBQ2pCLElBQUksQ0FBQyxNQUFNLEdBQUcsQ0FBQyxRQUF1QixFQUFFLE9BQWEsRUFBRSxFQUFFO2dCQUV4RCxJQUFJLENBQUMsU0FBUyxHQUFHLFFBQVEsQ0FBQztnQkFDMUIsSUFBSSxDQUFDLEtBQUssR0FBRyxPQUFPLENBQUM7Z0JBRXJCLElBQUksTUFBbUIsQ0FBQztnQkFDeEIsTUFBTSxHQUFHO29CQUNSLE9BQU8sRUFBRSxHQUFHLEVBQUU7d0JBQ2IsSUFBSSxDQUFDLFNBQVMsR0FBRyxTQUFTLENBQUM7d0JBQzNCLElBQUksQ0FBQyxLQUFLLEdBQUcsU0FBUyxDQUFDO29CQUN4QixDQUFDO2lCQUNELENBQUM7Z0JBQ0YsT0FBTyxNQUFNLENBQUM7WUFDZixDQUFDLENBQUM7U0FDRjtRQUNELE9BQU8sSUFBSSxDQUFDLE1BQU0sQ0FBQztJQUNwQixDQUFDO0lBRUQsSUFBSSxDQUFDLEtBQVE7UUFDWixJQUFJLElBQUksQ0FBQyxTQUFTLEVBQUU7WUFDbkIsSUFBSTtnQkFDSCxJQUFJLENBQUMsU0FBUyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsS0FBSyxFQUFFLEtBQUssQ0FBQyxDQUFDO2FBQ3ZDO1lBQUMsT0FBTyxDQUFDLEVBQUU7YUFDWDtTQUNEO0lBQ0YsQ0FBQztJQUVELFdBQVc7UUFDVixPQUFPLENBQUMsQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDO0lBQ3pCLENBQUM7SUFFRCxPQUFPO1FBQ04sSUFBSSxDQUFDLFNBQVMs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QWdCLEVBQUU7NEJBQ2hDLElBQUksQ0FBQyxjQUFjLEdBQUcsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLENBQUM7eUJBQy9CO3FCQUNEO29CQUNELElBQUksQ0FBQyxRQUFRLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsR0FBRyxHQUFHLGNBQWMsQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFDLENBQUM7b0JBQzFFLFNBQVM7aUJBQ1Q7YUFDRDtZQUNELE1BQU07U0FDTjtJQUNGLENBQUM7O0FBdktGLHdDQXdLQztBQXRLZSx1QkFBUSxHQUFHLFVBQVUsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4gKiAgQ29weXJpZ2h0IChjKSBNaWNyb3NvZnQgQ29ycG9yYXRpb24uIEFsbCByaWdodHMgcmVzZXJ2ZWQuXG4gKiAgTGljZW5zZWQgdW5kZXIgdGhlIE1JVCBMaWNlbnNlLiBTZWUgTGljZW5zZS50eHQgaW4gdGhlIHByb2plY3Qgcm9vdCBmb3IgbGljZW5zZSBpbmZvcm1hdGlvbi5cbiAqLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0qL1xuXG5pbXBvcnQgKiBhcyBlZSBmcm9tICdldmVudHMnO1xuaW1wb3J0IHsgRGVidWdQcm90b2NvbCB9IGZyb20gJ3ZzY29kZS1kZWJ1Z3Byb3RvY29sJztcbmltcG9ydCB7IFJlc3BvbnNlLCBFdmVudCB9IGZyb20gJy4vbWVzc2FnZXMnO1xuXG5pbnRlcmZhY2UgRGVidWdQcm90b2NvbE1lc3NhZ2Uge1xufVxuXG5pbnRlcmZhY2UgSURpc3Bvc2FibGUge1xuXHRkaXNwb3NlKCk6IHZvaWQ7XG59XG5cbmNsYXNzIERpc3Bvc2FibGUwIGltcGxlbWVudHMgSURpc3Bvc2FibGUge1xuXHRkaXNwb3NlKCk6IGFueSB7XG5cdH1cbn1cblxuaW50ZXJmYWNlIEV2ZW50MDxUPiB7XG5cdChsaXN0ZW5lcjogKGU6IFQpID0+IGFueSwgdGhpc0FyZz86IGFueSk6IERpc3Bvc2FibGUwO1xufVxuXG5jbGFzcyBFbWl0dGVyPFQ+IHtcblxuXHRwcml2YXRlIF9ldmVudD86IEV2ZW50MDxUPjtcblx0cHJpdmF0ZSBfbGlzdGVuZXI/OiAoZTogVCkgPT4gdm9pZDtcblx0cHJpdmF0ZSBfdGhpcz86IGFueTtcblxuXHRnZXQgZXZlbnQoKTogRXZlbnQwPFQ+IHtcblx0XHRpZiAoIXRoaXMuX2V2ZW50KSB7XG5cdFx0XHR0aGlzLl9ldmVudCA9IChsaXN0ZW5lcjogKGU6IFQpID0+IGFueSwgdGhpc0FyZz86IGFueSkgPT4ge1xuXG5cdFx0XHRcdHRoaXMuX2xpc3RlbmVyID0gbGlzdGVuZXI7XG5cdFx0XHRcdHRoaXMuX3RoaXMgPSB0aGlzQXJnO1xuXG5cdFx0XHRcdGxldCByZXN1bHQ6IElEaXNwb3NhYmxlO1xuXHRcdFx0XHRyZXN1bHQgPSB7XG5cdFx0XHRcdFx0ZGlzcG9zZTogKCkgPT4ge1xuXHRcdFx0XHRcdFx0dGhpcy5fbGlzdGVuZXIgPSB1bmRlZmluZWQ7XG5cdFx0XHRcdFx0XHR0aGlzLl90aGlzID0gdW5kZWZpbmVkO1xuXHRcdFx0XHRcdH1cblx0XHRcdFx0fTtcblx0XHRcdFx0cmV0dXJuIHJlc3VsdDtcblx0XHRcdH07XG5cdFx0fVxuXHRcdHJldHVybiB0aGlzLl9ldmVudDtcblx0fVxuXG5cdGZpcmUoZXZlbnQ6IFQpOiB2b2lkIHtcblx0XHRpZiAodGhpcy5fbGlzdGVuZXIpIHtcblx0XHRcdHRyeSB7XG5cdFx0XHRcdHRoaXMuX2xpc3RlbmVyLmNhbGwodGhpcy5fdGhpcywgZXZlbnQpO1xuXHRcdFx0fSBjYXRjaCAoZSkge1xuXHRcdFx0fVxuXHRcdH1cblx0fVxuXG5cdGhhc0xpc3RlbmVyKCkgOiBib29sZWFuIHtcblx0XHRyZXR1cm4gISF0aGlzLl9saXN0ZW5lcjtcblx0fVxuXG5cdGRpc3Bvc2UoKSB7XG5cdFx0dGhpcy5fbGlzdGVuZXIgPSB1bmRlZmluZWQ7XG5cdFx0dGhpcy5fdGhpcyA9IHVuZGVmaW5lZDtcblx0fVxufVxuXG4vKipcbiAqIEEgc3RydWN0dXJhbGx5IGVxdWl2YWxlbnQgY29weSBvZiB2c2NvZGUuRGVidWdBZGFwdGVyXG4gKi9cbmludGVyZmFjZSBWU0NvZGVEZWJ1Z0FkYXB0ZXIgZXh0ZW5kcyBEaXNwb3NhYmxlMCB7XG5cblx0cmVhZG9ubHkgb25EaWRTZW5kTWVzc2FnZTogRXZlbnQwPERlYnVnUHJvdG9jb2xNZXNzYWdlPjtcblxuXHRoYW5kbGVNZXNzYWdlKG1lc3NhZ2U6IERlYnVnUHJvdG9jb2wuUHJvdG9jb2xNZXNzYWdlKTogdm9pZDtcbn1cblxuZXhwb3J0IGNsYXNzIFByb3RvY29sU2VydmVyIGV4dGVuZHMgZWUuRXZlbnRFbWl0dGVyIGltcGxlbWVudHMgVlNDb2RlRGVidWdBZGFwdGVyIHtcblxuXHRwcml2YXRlIHN0YXRpYyBUV09fQ1JMRiA9ICdcXHJcXG5cXHJcXG4nO1xuXG5cdHByaXZhdGUgX3NlbmRNZXNzYWdlID0gbmV3IEVtaXR0ZXI8RGVidWdQcm90b2NvbE1lc3NhZ2U+KCk7XG5cblx0cHJpdmF0ZSBfcmF3RGF0YTogQnVmZmVyO1xuXHRwcml2YXRlIF9jb250ZW50TGVuZ3RoOiBudW1iZXI7XG5cdHByaXZhdGUgX3NlcXVlbmNlOiBudW1iZXI7XG5cdHByaXZhdGUgX3dyaXRhYmxlU3RyZWFtOiBOb2RlSlMuV3JpdGFibGVTdHJlYW07XG5cdHByaXZhdGUgX3BlbmRpbmdSZXF1ZXN0cyA9IG5ldyBNYXA8bnVtYmVyLCAocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuUmVzcG9uc2UpID0+IHZvaWQ+KCk7XG5cblx0Y29uc3RydWN0b3IoKSB7XG5cdFx0c3VwZXIoKTtcblx0fVxuXG5cdC8vIC0tLS0gaW1wbGVtZW50cyB2c2NvZGUuRGVidWdhZGFwdGVyIGludGVyZmFjZSAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cblxuXHRwdWJsaWMgZGlzcG9zZSgpOiBhbnkge1xuXHR9XG5cblx0cHVibGljIG9uRGlkU2VuZE1lc3NhZ2U6IEV2ZW50MDxEZWJ1Z1Byb3RvY29sTWVzc2FnZT4gPSB0aGlzLl9zZW5kTWVzc2FnZS5ldmVudDtcblxuXHRwdWJsaWMgaGFuZGxlTWVzc2FnZShtc2c6IERlYnVnUHJvdG9jb2wuUHJvdG9jb2xNZXNzYWdlKTogdm9pZCB7XG5cdFx0aWYgKG1zZy50eXBlID09PSAncmVxdWVzdCcpIHtcblx0XHRcdHRoaXMuZGlzcGF0Y2hSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlJlcXVlc3Q+bXNnKTtcblx0XHR9IGVsc2UgaWYgKG1zZy50eXBlID09PSAncmVzcG9uc2UnKSB7XG5cdFx0XHRjb25zdCByZXNwb25zZSA9IDxEZWJ1Z1Byb3RvY29sLlJlc3BvbnNlPm1zZztcblx0XHRcdGNvbnN0IGNsYiA9IHRoaXMuX3BlbmRpbmdSZXF1ZXN0cy5nZXQocmVzcG9uc2UucmVxdWVzdF9zZXEpO1xuXHRcdFx0aWYgKGNsYikge1xuXHRcdFx0XHR0aGlzLl9wZW5kaW5nUmVxdWVzdHMuZGVsZXRlKHJlc3BvbnNlLnJlcXVlc3Rfc2VxKTtcblx0XHRcdFx0Y2xiKHJlc3BvbnNlKTtcblx0XHRcdH1cblx0XHR9XG5cdH1cblxuXHRwcm90ZWN0ZWQgX2lzUnVubmluZ0lubGluZSgpIHtcblx0XHRyZXR1cm4gdGhpcy5fc2VuZE1lc3NhZ2UgJiYgdGhpcy5fc2VuZE1lc3NhZ2UuaGFzTGlzdGVuZXIoKTtcblx0fVxuXG5cdC8vLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cblxuXHRwdWJsaWMgc3RhcnQoaW5TdHJlYW06IE5vZGVKUy5SZWFkYWJsZVN0cmVhbSwgb3V0U3RyZWFtOiBOb2RlSlMuV3JpdGFibGVTdHJlYW0pOiB2b2lkIHtcblx0XHR0aGlzLl9zZXF1ZW5jZSA9IDE7XG5cdFx0dGhpcy5fd3JpdGFibGVTdHJlYW0gPSBvdXRTdHJlYW07XG5cdFx0dGhpcy5fcmF3RGF0YSA9IG5ldyBCdWZmZXIoMCk7XG5cblx0XHRpblN0cmVhbS5vbignZGF0YScsIChkYXRhOiBCdWZmZXIpID0+IHRoaXMuX2hhbmRsZURhdGEoZGF0YSkpO1xuXG5cdFx0aW5TdHJlYW0ub24oJ2Nsb3NlJywgKCkgPT4ge1xuXHRcdFx0dGhpcy5fZW1pdEV2ZW50KG5ldyBFdmVudCgnY2xvc2UnKSk7XG5cdFx0fSk7XG5cdFx0aW5TdHJlYW0ub24oJ2Vycm9yJywgKGVycm9yKSA9PiB7XG5cdFx0XHR0aGlzLl9lbWl0RXZlbnQobmV3IEV2ZW50KCdlcnJvcicsICdpblN0cmVhbSBlcnJvcjogJyArIChlcnJvciAmJiBlcnJvci5tZXNzYWdlKSkpO1xuXHRcdH0pO1xuXG5cdFx0b3V0U3RyZWFtLm9uKCdlcnJvcicsIChlcnJvcikgPT4ge1xuXHRcdFx0dGhpcy5fZW1pdEV2ZW50KG5ldyBFdmVudCgnZXJyb3InLCAnb3V0U3RyZWFtIGVycm9yOiAnICsgKGVycm9yICYmIGVycm9yLm1lc3NhZ2UpKSk7XG5cdFx0fSk7XG5cblx0XHRpblN0cmVhbS5yZXN1bWUoKTtcblx0fVxuXG5cdHB1YmxpYyBzdG9wKCk6IHZvaWQge1xuXHRcdGlmICh0aGlzLl93cml0YWJsZVN0cmVhbSkge1xuXHRcdFx0dGhpcy5fd3JpdGFibGVTdHJlYW0uZW5kKCk7XG5cdFx0fVxuXHR9XG5cblx0cHVibGljIHNlbmRFdmVudChldmVudDogRGVidWdQcm90b2NvbC5FdmVudCk6IHZvaWQge1xuXHRcdHRoaXMuX3NlbmQoJ2V2ZW50JywgZXZlbnQpO1xuXHR9XG5cblx0cHVibGljIHNlbmRSZXNwb25zZShyZXNwb25zZTogRGVidWdQcm90b2NvbC5SZXNwb25zZSk6IHZvaWQge1xuXHRcdGlmIChyZXNwb25zZS5zZXEgPiAwKSB7XG5cdFx0XHRjb25zb2xlLmVycm9yKGBhdHRlbXB0IHRvIHNlbmQgbW9yZSB0aGFuIG9uZSByZXNwb25zZSBmb3IgY29tbWFuZCAke3Jlc3BvbnNlLmNvbW1hbmR9YCk7XG5cdFx0fSBlbHNlIHtcblx0XHRcdHRoaXMuX3NlbmQoJ3Jlc3BvbnNlJywgcmVzcG9uc2UpO1xuXHRcdH1cblx0fVxuXG5cdHB1YmxpYyBzZW5kUmVxdWVzdChjb21tYW5kOiBzdHJpbmcsIGFyZ3M6IGFueSwgdGltZW91dDogbnVtYmVyLCBjYjogKHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlJlc3BvbnNlKSA9PiB2b2lkKSA6IHZvaWQge1xuXG5cdFx0Y29uc3QgcmVxdWVzdDogYW55ID0ge1xuXHRcdFx0Y29tbWFuZDogY29tbWFuZFxuXHRcdH07XG5cdFx0aWYgKGFyZ3MgJiYgT2JqZWN0LmtleXMoYXJncykubGVuZ3RoID4gMCkge1xuXHRcdFx0cmVxdWVzdC5hcmd1bWVudHMgPSBhcmdzO1xuXHRcdH1cblxuXHRcdHRoaXMuX3NlbmQoJ3JlcXVlc3QnLCByZXF1ZXN0KTtcblxuXHRcdGlmIChjYikge1xuXHRcdFx0dGhpcy5fcGVuZGluZ1JlcXVlc3RzLnNldChyZXF1ZXN0LnNlcSwgY2IpO1xuXG5cdFx0XHRjb25zdCB0aW1lciA9IHNldFRpbWVvdXQoKCkgPT4ge1xuXHRcdFx0XHRjbGVhclRpbWVvdXQodGltZXIpO1xuXHRcdFx0XHRjb25zdCBjbGIgPSB0aGlzLl9wZW5kaW5nUmVxdWVzdHMuZ2V0KHJlcXVlc3Quc2VxKTtcblx0XHRcdFx0aWYgKGNsYikge1xuXHRcdFx0XHRcdHRoaXMuX3BlbmRpbmdSZXF1ZXN0cy5kZWxldGUocmVxdWVzdC5zZXEpO1xuXHRcdFx0XHRcdGNsYihuZXcgUmVzcG9uc2UocmVxdWVzdCwgJ3RpbWVvdXQnKSk7XG5cdFx0XHRcdH1cblx0XHRcdH0sIHRpbWVvdXQpO1xuXHRcdH1cblx0fVxuXG5cdC8vIC0tLS0gcHJvdGVjdGVkIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cblxuXHRwcm90ZWN0ZWQgZGlzcGF0Y2hSZXF1ZXN0KHJlcXVlc3Q6IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHR9XG5cblx0Ly8gLS0tLSBwcml2YXRlIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuXG5cdHByaXZhdGUgX2VtaXRFdmVudChldmVudDogRGVidWdQcm90b2NvbC5FdmVudCkge1xuXHRcdHRoaXMuZW1pdChldmVudC5ldmVudCwgZXZlbnQpO1xuXHR9XG5cblx0cHJpdmF0ZSBfc2VuZCh0eXA6ICdyZXF1ZXN0JyB8ICdyZXNwb25zZScgfCAnZXZlbnQnLCBtZXNzYWdlOiBEZWJ1Z1Byb3RvY29sLlByb3RvY29sTWVzc2FnZSk6IHZvaWQge1xuXG5cdFx0bWVzc2FnZS50eXBlID0gdHlwO1xuXHRcdG1lc3NhZ2Uuc2VxID0gdGhpcy5fc2VxdWVuY2UrKztcblxuXHRcdGlmICh0aGlzLl93cml0YWJsZVN0cmVhbSkge1xuXHRcdFx0Y29uc3QganNvbiA9IEpTT04uc3RyaW5naWZ5KG1lc3NhZ2UpO1xuXHRcdFx0dGhpcy5fd3JpdGFibGVTdHJlYW0ud3JpdGUoYENvbnRlbnQtTGVuZ3RoOiAke0J1ZmZlci5ieXRlTGVuZ3RoKGpzb24sICd1dGY4Jyl9XFxyXFxuXFxyXFxuJHtqc29ufWAsICd1dGY4Jyk7XG5cdFx0fVxuXHRcdHRoaXMuX3NlbmRNZXNzYWdlLmZpcmUobWVzc2FnZSk7XG5cdH1cblxuXHRwcml2YXRlIF9oYW5kbGVEYXRhKGRhdGE6IEJ1ZmZlcik6IHZvaWQge1xuXG5cdFx0dGhpcy5fcmF3RGF0YSA9IEJ1ZmZlci5jb25jYXQoW3RoaXMuX3Jhd0RhdGEsIGRhdGFdKTtcblxuXHRcdHdoaWxlICh0cnVlKSB7XG5cdFx0XHRpZiAodGhpcy5fY29udGVudExlbmd0aCA+PSAwKSB7XG5cdFx0XHRcdGlmICh0aGlzLl9yYXdEYXRhLmxlbmd0aCA+PSB0aGlzLl9jb250ZW50TGVuZ3RoKSB7XG5cdFx0XHRcdFx0Y29uc3QgbWVzc2FnZSA9IHRoaXMuX3Jhd0RhdGEudG9TdHJpbmcoJ3V0ZjgnLCAwLCB0aGlzLl9jb250ZW50TGVuZ3RoKTtcblx0XHRcdFx0XHR0aGlzLl9yYXdEYXRhID0gdGhpcy5fcmF3RGF0YS5zbGljZSh0aGlzLl9jb250ZW50TGVuZ3RoKTtcblx0XHRcdFx0XHR0aGlzLl9jb250ZW50TGVuZ3RoID0gLTE7XG5cdFx0XHRcdFx0aWYgKG1lc3NhZ2UubGVuZ3RoID4gMCkge1xuXHRcdFx0XHRcdFx0dHJ5IHtcblx0XHRcdFx0XHRcdFx0bGV0IG1zZzogRGVidWdQcm90b2NvbC5Qcm90b2NvbE1lc3NhZ2UgPSBKU09OLnBhcnNlKG1lc3NhZ2UpO1xuXHRcdFx0XHRcdFx0XHR0aGlzLmhhbmRsZU1lc3NhZ2UobXNnKTtcblx0XHRcdFx0XHRcdH1cblx0XHRcdFx0XHRcdGNhdGNoIChlKSB7XG5cdFx0XHRcdFx0XHRcdHRoaXMuX2VtaXRFdmVudChuZXcgRXZlbnQoJ2Vycm9yJywgJ0Vycm9yIGhhbmRsaW5nIGRhdGE6ICcgKyAoZSAmJiBlLm1lc3NhZ2UpKSk7XG5cdFx0XHRcdFx0XHR9XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHRcdGNvbnRpbnVlO1x0Ly8gdGhlcmUgbWF5IGJlIG1vcmUgY29tcGxldGUgbWVzc2FnZXMgdG8gcHJvY2Vzc1xuXHRcdFx0XHR9XG5cdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRjb25zdCBpZHggPSB0aGlzLl9yYXdEYXRhLmluZGV4T2YoUHJvdG9jb2xTZXJ2ZXIuVFdPX0NSTEYpO1xuXHRcdFx0XHRpZiAoaWR4ICE9PSAtMSkge1xuXHRcdFx0XHRcdGNvbnN0IGhlYWRlciA9IHRoaXMuX3Jhd0RhdGEudG9TdHJpbmcoJ3V0ZjgnLCAwLCBpZHgpO1xuXHRcdFx0XHRcdGNvbnN0IGxpbmVzID0gaGVhZGVyLnNwbGl0KCdcXHJcXG4nKTtcblx0XHRcdFx0XHRmb3IgKGxldCBpID0gMDsgaSA8IGxpbmVzLmxlbmd0aDsgaSsrKSB7XG5cdFx0XHRcdFx0XHRjb25zdCBwYWlyID0gbGluZXNbaV0uc3BsaXQoLzogKy8pO1xuXHRcdFx0XHRcdFx0aWYgKHBhaXJbMF0gPT0gJ0NvbnRlbnQtTGVuZ3RoJykge1xuXHRcdFx0XHRcdFx0XHR0aGlzLl9jb250ZW50TGVuZ3RoID0gK3BhaXJbMV07XG5cdFx0XHRcdFx0XHR9XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHRcdHRoaXMuX3Jhd0RhdGEgPSB0aGlzLl9yYXdEYXRhLnNsaWNlKGlkeCArIFByb3RvY29sU2VydmVyLlRXT19DUkxGLmxlbmd0aCk7XG5cdFx0XHRcdFx0Y29udGludWU7XG5cdFx0XHRcdH1cblx0XHRcdH1cblx0XHRcdGJyZWFrO1xuXHRcdH1cblx0fVxufVxuIl19

/***/ }),

/***/ 133:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.runDebugAdapter = void 0;
const Net = __webpack_require__(26);
function runDebugAdapter(debugSession) {
    // parse arguments
    let port = 0;
    const args = process.argv.slice(2);
    args.forEach(function (val, index, array) {
        const portMatch = /^--server=(\d{4,5})$/.exec(val);
        if (portMatch) {
            port = parseInt(portMatch[1], 10);
        }
    });
    if (port > 0) {
        // start as a server
        console.error(`waiting for debug protocol on port ${port}`);
        Net.createServer((socket) => {
            console.error('>> accepted connection from client');
            socket.on('end', () => {
                console.error('>> client connection closed\n');
            });
            const session = new debugSession(false, true);
            session.setRunAsServer(true);
            session.start(socket, socket);
        }).listen(port);
    }
    else {
        // start a session
        //console.error('waiting for debug protocol on stdin/stdout');
        const session = new debugSession(false);
        process.on('SIGTERM', () => {
            session.shutdown();
        });
        session.start(process.stdin, process.stdout);
    }
}
exports.runDebugAdapter = runDebugAdapter;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoicnVuRGVidWdBZGFwdGVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vc3JjL3J1bkRlYnVnQWRhcHRlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUE7OztnR0FHZ0c7OztBQUVoRywyQkFBMkI7QUFJM0IsU0FBZ0IsZUFBZSxDQUFDLFlBQWlDO0lBRWhFLGtCQUFrQjtJQUNsQixJQUFJLElBQUksR0FBRyxDQUFDLENBQUM7SUFDYixNQUFNLElBQUksR0FBRyxPQUFPLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUMsQ0FBQztJQUNuQyxJQUFJLENBQUMsT0FBTyxDQUFDLFVBQVUsR0FBRyxFQUFFLEtBQUssRUFBRSxLQUFLO1FBQ3ZDLE1BQU0sU0FBUyxHQUFHLHNCQUFzQixDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUNuRCxJQUFJLFNBQVMsRUFBRTtZQUNkLElBQUksR0FBRyxRQUFRLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxDQUFDO1NBQ2xDO0lBQ0YsQ0FBQyxDQUFDLENBQUM7SUFFSCxJQUFJLElBQUksR0FBRyxDQUFDLEVBQUU7UUFDYixvQkFBb0I7UUFDcEIsT0FBTyxDQUFDLEtBQUssQ0FBQyxzQ0FBc0MsSUFBSSxFQUFFLENBQUMsQ0FBQztRQUM1RCxHQUFHLENBQUMsWUFBWSxDQUFDLENBQUMsTUFBTSxFQUFFLEVBQUU7WUFDM0IsT0FBTyxDQUFDLEtBQUssQ0FBQyxvQ0FBb0MsQ0FBQyxDQUFDO1lBQ3BELE1BQU0sQ0FBQyxFQUFFLENBQUMsS0FBSyxFQUFFLEdBQUcsRUFBRTtnQkFDckIsT0FBTyxDQUFDLEtBQUssQ0FBQywrQkFBK0IsQ0FBQyxDQUFDO1lBQ2hELENBQUMsQ0FBQyxDQUFDO1lBQ0gsTUFBTSxPQUFPLEdBQUcsSUFBSSxZQUFZLENBQUMsS0FBSyxFQUFFLElBQUksQ0FBQyxDQUFDO1lBQzlDLE9BQU8sQ0FBQyxjQUFjLENBQUMsSUFBSSxDQUFDLENBQUM7WUFDN0IsT0FBTyxDQUFDLEtBQUssQ0FBQyxNQUFNLEVBQUUsTUFBTSxDQUFDLENBQUM7UUFDL0IsQ0FBQyxDQUFDLENBQUMsTUFBTSxDQUFDLElBQUksQ0FBQyxDQUFDO0tBQ2hCO1NBQU07UUFFTixrQkFBa0I7UUFDbEIsOERBQThEO1FBQzlELE1BQU0sT0FBTyxHQUFHLElBQUksWUFBWSxDQUFDLEtBQUssQ0FBQyxDQUFDO1FBQ3hDLE9BQU8sQ0FBQyxFQUFFLENBQUMsU0FBUyxFQUFFLEdBQUcsRUFBRTtZQUMxQixPQUFPLENBQUMsUUFBUSxFQUFFLENBQUM7UUFDcEIsQ0FBQyxDQUFDLENBQUM7UUFDSCxPQUFPLENBQUMsS0FBSyxDQUFDLE9BQU8sQ0FBQyxLQUFLLEVBQUUsT0FBTyxDQUFDLE1BQU0sQ0FBQyxDQUFDO0tBQzdDO0FBQ0YsQ0FBQztBQWxDRCwwQ0FrQ0MiLCJzb3VyY2VzQ29udGVudCI6WyIvKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuICogIENvcHlyaWdodCAoYykgTWljcm9zb2Z0IENvcnBvcmF0aW9uLiBBbGwgcmlnaHRzIHJlc2VydmVkLlxuICogIExpY2Vuc2VkIHVuZGVyIHRoZSBNSVQgTGljZW5zZS4gU2VlIExpY2Vuc2UudHh0IGluIHRoZSBwcm9qZWN0IHJvb3QgZm9yIGxpY2Vuc2UgaW5mb3JtYXRpb24uXG4gKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tKi9cblxuaW1wb3J0ICogYXMgTmV0IGZyb20gJ25ldCc7XG5cbmltcG9ydCB7IERlYnVnU2Vzc2lvbiB9IGZyb20gJy4vZGVidWdTZXNzaW9uJztcblxuZXhwb3J0IGZ1bmN0aW9uIHJ1bkRlYnVnQWRhcHRlcihkZWJ1Z1Nlc3Npb246IHR5cGVvZiBEZWJ1Z1Nlc3Npb24pIHtcblxuXHQvLyBwYXJzZSBhcmd1bWVudHNcblx0bGV0IHBvcnQgPSAwO1xuXHRjb25zdCBhcmdzID0gcHJvY2Vzcy5hcmd2LnNsaWNlKDIpO1xuXHRhcmdzLmZvckVhY2goZnVuY3Rpb24gKHZhbCwgaW5kZXgsIGFycmF5KSB7XG5cdFx0Y29uc3QgcG9ydE1hdGNoID0gL14tLXNlcnZlcj0oXFxkezQsNX0pJC8uZXhlYyh2YWwpO1xuXHRcdGlmIChwb3J0TWF0Y2gpIHtcblx0XHRcdHBvcnQgPSBwYXJzZUludChwb3J0TWF0Y2hbMV0sIDEwKTtcblx0XHR9XG5cdH0pO1xuXG5cdGlmIChwb3J0ID4gMCkge1xuXHRcdC8vIHN0YXJ0IGFzIGEgc2VydmVyXG5cdFx0Y29uc29sZS5lcnJvcihgd2FpdGluZyBmb3IgZGVidWcgcHJvdG9jb2wgb24gcG9ydCAke3BvcnR9YCk7XG5cdFx0TmV0LmNyZWF0ZVNlcnZlcigoc29ja2V0KSA9PiB7XG5cdFx0XHRjb25zb2xlLmVycm9yKCc+PiBhY2NlcHRlZCBjb25uZWN0aW9uIGZyb20gY2xpZW50Jyk7XG5cdFx0XHRzb2NrZXQub24oJ2VuZCcsICgpID0+IHtcblx0XHRcdFx0Y29uc29sZS5lcnJvcignPj4gY2xpZW50IGNvbm5lY3Rpb24gY2xvc2VkXFxuJyk7XG5cdFx0XHR9KTtcblx0XHRcdGNvbnN0IHNlc3Npb24gPSBuZXcgZGVidWdTZXNzaW9uKGZhbHNlLCB0cnVlKTtcblx0XHRcdHNlc3Npb24uc2V0UnVuQXNTZXJ2ZXIodHJ1ZSk7XG5cdFx0XHRzZXNzaW9uLnN0YXJ0KHNvY2tldCwgc29ja2V0KTtcblx0XHR9KS5saXN0ZW4ocG9ydCk7XG5cdH0gZWxzZSB7XG5cblx0XHQvLyBzdGFydCBhIHNlc3Npb25cblx0XHQvL2NvbnNvbGUuZXJyb3IoJ3dhaXRpbmcgZm9yIGRlYnVnIHByb3RvY29sIG9uIHN0ZGluL3N0ZG91dCcpO1xuXHRcdGNvbnN0IHNlc3Npb24gPSBuZXcgZGVidWdTZXNzaW9uKGZhbHNlKTtcblx0XHRwcm9jZXNzLm9uKCdTSUdURVJNJywgKCkgPT4ge1xuXHRcdFx0c2Vzc2lvbi5zaHV0ZG93bigpO1xuXHRcdH0pO1xuXHRcdHNlc3Npb24uc3RhcnQocHJvY2Vzcy5zdGRpbiwgcHJvY2Vzcy5zdGRvdXQpO1xuXHR9XG59XG4iXX0=

/***/ }),

/***/ 134:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.LoggingDebugSession = void 0;
const Logger = __webpack_require__(48);
const logger = Logger.logger;
const debugSession_1 = __webpack_require__(46);
class LoggingDebugSession extends debugSession_1.DebugSession {
    constructor(obsolete_logFilePath, obsolete_debuggerLinesAndColumnsStartAt1, obsolete_isServer) {
        super(obsolete_debuggerLinesAndColumnsStartAt1, obsolete_isServer);
        this.obsolete_logFilePath = obsolete_logFilePath;
        this.on('error', (event) => {
            logger.error(event.body);
        });
    }
    start(inStream, outStream) {
        super.start(inStream, outStream);
        logger.init(e => this.sendEvent(e), this.obsolete_logFilePath, this._isServer);
    }
    /**
     * Overload sendEvent to log
     */
    sendEvent(event) {
        if (!(event instanceof Logger.LogOutputEvent)) {
            // Don't create an infinite loop...
            let objectToLog = event;
            if (event instanceof debugSession_1.OutputEvent && event.body && event.body.data && event.body.data.doNotLogOutput) {
                delete event.body.data.doNotLogOutput;
                objectToLog = Object.assign({}, event);
                objectToLog.body = Object.assign(Object.assign({}, event.body), { output: '<output not logged>' });
            }
            logger.verbose(`To client: ${JSON.stringify(objectToLog)}`);
        }
        super.sendEvent(event);
    }
    /**
     * Overload sendRequest to log
     */
    sendRequest(command, args, timeout, cb) {
        logger.verbose(`To client: ${JSON.stringify(command)}(${JSON.stringify(args)}), timeout: ${timeout}`);
        super.sendRequest(command, args, timeout, cb);
    }
    /**
     * Overload sendResponse to log
     */
    sendResponse(response) {
        logger.verbose(`To client: ${JSON.stringify(response)}`);
        super.sendResponse(response);
    }
    dispatchRequest(request) {
        logger.verbose(`From client: ${request.command}(${JSON.stringify(request.arguments)})`);
        super.dispatchRequest(request);
    }
}
exports.LoggingDebugSession = LoggingDebugSession;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibG9nZ2luZ0RlYnVnU2Vzc2lvbi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uL3NyYy9sb2dnaW5nRGVidWdTZXNzaW9uLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQTs7O2dHQUdnRzs7O0FBSWhHLG1DQUFtQztBQUNuQyxNQUFNLE1BQU0sR0FBRyxNQUFNLENBQUMsTUFBTSxDQUFDO0FBQzdCLGlEQUF5RDtBQUV6RCxNQUFhLG1CQUFvQixTQUFRLDJCQUFZO0lBQ3BELFlBQTJCLG9CQUE2QixFQUFFLHdDQUFrRCxFQUFFLGlCQUEyQjtRQUN4SSxLQUFLLENBQUMsd0NBQXdDLEVBQUUsaUJBQWlCLENBQUMsQ0FBQztRQUR6Qyx5QkFBb0IsR0FBcEIsb0JBQW9CLENBQVM7UUFHdkQsSUFBSSxDQUFDLEVBQUUsQ0FBQyxPQUFPLEVBQUUsQ0FBQyxLQUEwQixFQUFFLEVBQUU7WUFDL0MsTUFBTSxDQUFDLEtBQUssQ0FBQyxLQUFLLENBQUMsSUFBSSxDQUFDLENBQUM7UUFDMUIsQ0FBQyxDQUFDLENBQUM7SUFDSixDQUFDO0lBRU0sS0FBSyxDQUFDLFFBQStCLEVBQUUsU0FBZ0M7UUFDN0UsS0FBSyxDQUFDLEtBQUssQ0FBQyxRQUFRLEVBQUUsU0FBUyxDQUFDLENBQUM7UUFDakMsTUFBTSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsRUFBRSxDQUFDLElBQUksQ0FBQyxTQUFTLENBQUMsQ0FBQyxDQUFDLEVBQUUsSUFBSSxDQUFDLG9CQUFvQixFQUFFLElBQUksQ0FBQyxTQUFTLENBQUMsQ0FBQztJQUNoRixDQUFDO0lBRUQ7O09BRUc7SUFDSSxTQUFTLENBQUMsS0FBMEI7UUFDMUMsSUFBSSxDQUFDLENBQUMsS0FBSyxZQUFZLE1BQU0sQ0FBQyxjQUFjLENBQUMsRUFBRTtZQUM5QyxtQ0FBbUM7WUFFbkMsSUFBSSxXQUFXLEdBQUcsS0FBSyxDQUFDO1lBQ3hCLElBQUksS0FBSyxZQUFZLDBCQUFXLElBQUksS0FBSyxDQUFDLElBQUksSUFBSSxLQUFLLENBQUMsSUFBSSxDQUFDLElBQUksSUFBSSxLQUFLLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxjQUFjLEVBQUU7Z0JBQ3BHLE9BQU8sS0FBSyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsY0FBYyxDQUFDO2dCQUN0QyxXQUFXLHFCQUFRLEtBQUssQ0FBRSxDQUFDO2dCQUMzQixXQUFXLENBQUMsSUFBSSxtQ0FBUSxLQUFLLENBQUMsSUFBSSxLQUFFLE1BQU0sRUFBRSxxQkFBcUIsR0FBRSxDQUFBO2FBQ25FO1lBRUQsTUFBTSxDQUFDLE9BQU8sQ0FBQyxjQUFjLElBQUksQ0FBQyxTQUFTLENBQUMsV0FBVyxDQUFDLEVBQUUsQ0FBQyxDQUFDO1NBQzVEO1FBRUQsS0FBSyxDQUFDLFNBQVMsQ0FBQyxLQUFLLENBQUMsQ0FBQztJQUN4QixDQUFDO0lBRUQ7O09BRUc7SUFDSSxXQUFXLENBQUMsT0FBZSxFQUFFLElBQVMsRUFBRSxPQUFlLEVBQUUsRUFBOEM7UUFDN0csTUFBTSxDQUFDLE9BQU8sQ0FBQyxjQUFjLElBQUksQ0FBQyxTQUFTLENBQUMsT0FBTyxDQUFDLElBQUksSUFBSSxDQUFDLFNBQVMsQ0FBQyxJQUFJLENBQUMsZUFBZSxPQUFPLEVBQUUsQ0FBQyxDQUFDO1FBQ3RHLEtBQUssQ0FBQyxXQUFXLENBQUMsT0FBTyxFQUFFLElBQUksRUFBRSxPQUFPLEVBQUUsRUFBRSxDQUFDLENBQUM7SUFDL0MsQ0FBQztJQUVEOztPQUVHO0lBQ0ksWUFBWSxDQUFDLFFBQWdDO1FBQ25ELE1BQU0sQ0FBQyxPQUFPLENBQUMsY0FBYyxJQUFJLENBQUMsU0FBUyxDQUFDLFFBQVEsQ0FBQyxFQUFFLENBQUMsQ0FBQztRQUN6RCxLQUFLLENBQUMsWUFBWSxDQUFDLFFBQVEsQ0FBQyxDQUFDO0lBQzlCLENBQUM7SUFFUyxlQUFlLENBQUMsT0FBOEI7UUFDdkQsTUFBTSxDQUFDLE9BQU8sQ0FBQyxnQkFBZ0IsT0FBTyxDQUFDLE9BQU8sSUFBSSxJQUFJLENBQUMsU0FBUyxDQUFDLE9BQU8sQ0FBQyxTQUFTLENBQUUsR0FBRyxDQUFDLENBQUM7UUFDekYsS0FBSyxDQUFDLGVBQWUsQ0FBQyxPQUFPLENBQUMsQ0FBQztJQUNoQyxDQUFDO0NBQ0Q7QUF0REQsa0RBc0RDIiwic291cmNlc0NvbnRlbnQiOlsiLyotLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiAqICBDb3B5cmlnaHQgKGMpIE1pY3Jvc29mdCBDb3Jwb3JhdGlvbi4gQWxsIHJpZ2h0cyByZXNlcnZlZC5cbiAqICBMaWNlbnNlZCB1bmRlciB0aGUgTUlUIExpY2Vuc2UuIFNlZSBMaWNlbnNlLnR4dCBpbiB0aGUgcHJvamVjdCByb290IGZvciBsaWNlbnNlIGluZm9ybWF0aW9uLlxuICotLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLSovXG5cbmltcG9ydCB7RGVidWdQcm90b2NvbH0gZnJvbSAndnNjb2RlLWRlYnVncHJvdG9jb2wnO1xuXG5pbXBvcnQgKiBhcyBMb2dnZXIgZnJvbSAnLi9sb2dnZXInO1xuY29uc3QgbG9nZ2VyID0gTG9nZ2VyLmxvZ2dlcjtcbmltcG9ydCB7RGVidWdTZXNzaW9uLCBPdXRwdXRFdmVudH0gZnJvbSAnLi9kZWJ1Z1Nlc3Npb24nO1xuXG5leHBvcnQgY2xhc3MgTG9nZ2luZ0RlYnVnU2Vzc2lvbiBleHRlbmRzIERlYnVnU2Vzc2lvbiB7XG5cdHB1YmxpYyBjb25zdHJ1Y3Rvcihwcml2YXRlIG9ic29sZXRlX2xvZ0ZpbGVQYXRoPzogc3RyaW5nLCBvYnNvbGV0ZV9kZWJ1Z2dlckxpbmVzQW5kQ29sdW1uc1N0YXJ0QXQxPzogYm9vbGVhbiwgb2Jzb2xldGVfaXNTZXJ2ZXI/OiBib29sZWFuKSB7XG5cdFx0c3VwZXIob2Jzb2xldGVfZGVidWdnZXJMaW5lc0FuZENvbHVtbnNTdGFydEF0MSwgb2Jzb2xldGVfaXNTZXJ2ZXIpO1xuXG5cdFx0dGhpcy5vbignZXJyb3InLCAoZXZlbnQ6IERlYnVnUHJvdG9jb2wuRXZlbnQpID0+IHtcblx0XHRcdGxvZ2dlci5lcnJvcihldmVudC5ib2R5KTtcblx0XHR9KTtcblx0fVxuXG5cdHB1YmxpYyBzdGFydChpblN0cmVhbTogTm9kZUpTLlJlYWRhYmxlU3RyZWFtLCBvdXRTdHJlYW06IE5vZGVKUy5Xcml0YWJsZVN0cmVhbSk6IHZvaWQge1xuXHRcdHN1cGVyLnN0YXJ0KGluU3RyZWFtLCBvdXRTdHJlYW0pO1xuXHRcdGxvZ2dlci5pbml0KGUgPT4gdGhpcy5zZW5kRXZlbnQoZSksIHRoaXMub2Jzb2xldGVfbG9nRmlsZVBhdGgsIHRoaXMuX2lzU2VydmVyKTtcblx0fVxuXG5cdC8qKlxuXHQgKiBPdmVybG9hZCBzZW5kRXZlbnQgdG8gbG9nXG5cdCAqL1xuXHRwdWJsaWMgc2VuZEV2ZW50KGV2ZW50OiBEZWJ1Z1Byb3RvY29sLkV2ZW50KTogdm9pZCB7XG5cdFx0aWYgKCEoZXZlbnQgaW5zdGFuY2VvZiBMb2dnZXIuTG9nT3V0cHV0RXZlbnQpKSB7XG5cdFx0XHQvLyBEb24ndCBjcmVhdGUgYW4gaW5maW5pdGUgbG9vcC4uLlxuXG5cdFx0XHRsZXQgb2JqZWN0VG9Mb2cgPSBldmVudDtcblx0XHRcdGlmIChldmVudCBpbnN0YW5jZW9mIE91dHB1dEV2ZW50ICYmIGV2ZW50LmJvZHkgJiYgZXZlbnQuYm9keS5kYXRhICYmIGV2ZW50LmJvZHkuZGF0YS5kb05vdExvZ091dHB1dCkge1xuXHRcdFx0XHRkZWxldGUgZXZlbnQuYm9keS5kYXRhLmRvTm90TG9nT3V0cHV0O1xuXHRcdFx0XHRvYmplY3RUb0xvZyA9IHsgLi4uZXZlbnQgfTtcblx0XHRcdFx0b2JqZWN0VG9Mb2cuYm9keSA9IHsgLi4uZXZlbnQuYm9keSwgb3V0cHV0OiAnPG91dHB1dCBub3QgbG9nZ2VkPicgfVxuXHRcdFx0fVxuXG5cdFx0XHRsb2dnZXIudmVyYm9zZShgVG8gY2xpZW50OiAke0pTT04uc3RyaW5naWZ5KG9iamVjdFRvTG9nKX1gKTtcblx0XHR9XG5cblx0XHRzdXBlci5zZW5kRXZlbnQoZXZlbnQpO1xuXHR9XG5cblx0LyoqXG5cdCAqIE92ZXJsb2FkIHNlbmRSZXF1ZXN0IHRvIGxvZ1xuXHQgKi9cblx0cHVibGljIHNlbmRSZXF1ZXN0KGNvbW1hbmQ6IHN0cmluZywgYXJnczogYW55LCB0aW1lb3V0OiBudW1iZXIsIGNiOiAocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuUmVzcG9uc2UpID0+IHZvaWQpOiB2b2lkIHtcblx0XHRsb2dnZXIudmVyYm9zZShgVG8gY2xpZW50OiAke0pTT04uc3RyaW5naWZ5KGNvbW1hbmQpfSgke0pTT04uc3RyaW5naWZ5KGFyZ3MpfSksIHRpbWVvdXQ6ICR7dGltZW91dH1gKTtcblx0XHRzdXBlci5zZW5kUmVxdWVzdChjb21tYW5kLCBhcmdzLCB0aW1lb3V0LCBjYik7XG5cdH1cblxuXHQvKipcblx0ICogT3ZlcmxvYWQgc2VuZFJlc3BvbnNlIHRvIGxvZ1xuXHQgKi9cblx0cHVibGljIHNlbmRSZXNwb25zZShyZXNwb25zZTogRGVidWdQcm90b2NvbC5SZXNwb25zZSk6IHZvaWQge1xuXHRcdGxvZ2dlci52ZXJib3NlKGBUbyBjbGllbnQ6ICR7SlNPTi5zdHJpbmdpZnkocmVzcG9uc2UpfWApO1xuXHRcdHN1cGVyLnNlbmRSZXNwb25zZShyZXNwb25zZSk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgZGlzcGF0Y2hSZXF1ZXN0KHJlcXVlc3Q6IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdGxvZ2dlci52ZXJib3NlKGBGcm9tIGNsaWVudDogJHtyZXF1ZXN0LmNvbW1hbmR9KCR7SlNPTi5zdHJpbmdpZnkocmVxdWVzdC5hcmd1bWVudHMpIH0pYCk7XG5cdFx0c3VwZXIuZGlzcGF0Y2hSZXF1ZXN0KHJlcXVlc3QpO1xuXHR9XG59XG4iXX0=

/***/ }),

/***/ 135:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.InternalLogger = void 0;
const fs = __webpack_require__(6);
const path = __webpack_require__(2);
const mkdirp = __webpack_require__(136);
function mkdirpPromise(folder) {
    return new Promise((resolve, reject) => {
        mkdirp(folder, (err, made) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(made);
            }
        });
    });
}
const logger_1 = __webpack_require__(48);
/**
 * Manages logging, whether to console.log, file, or VS Code console.
 * Encapsulates the state specific to each logging session
 */
class InternalLogger {
    constructor(logCallback, isServer) {
        /** Dispose and allow exit to continue normally */
        this.beforeExitCallback = () => this.dispose();
        this._logCallback = logCallback;
        this._logToConsole = isServer;
        this._minLogLevel = logger_1.LogLevel.Warn;
        this.disposeCallback = (signal, code) => {
            this.dispose();
            // Exit with 128 + value of the signal code.
            // https://nodejs.org/api/process.html#process_exit_codes
            code = code || 2; // SIGINT
            code += 128;
            process.exit(code);
        };
    }
    setup(options) {
        return __awaiter(this, void 0, void 0, function* () {
            this._minLogLevel = options.consoleMinLogLevel;
            this._prependTimestamp = options.prependTimestamp;
            // Open a log file in the specified location. Overwritten on each run.
            if (options.logFilePath) {
                if (!path.isAbsolute(options.logFilePath)) {
                    this.log(`logFilePath must be an absolute path: ${options.logFilePath}`, logger_1.LogLevel.Error);
                }
                else {
                    const handleError = err => this.sendLog(`Error creating log file at path: ${options.logFilePath}. Error: ${err.toString()}\n`, logger_1.LogLevel.Error);
                    try {
                        yield mkdirpPromise(path.dirname(options.logFilePath));
                        this.log(`Verbose logs are written to:\n`, logger_1.LogLevel.Warn);
                        this.log(options.logFilePath + '\n', logger_1.LogLevel.Warn);
                        this._logFileStream = fs.createWriteStream(options.logFilePath);
                        this.logDateTime();
                        this.setupShutdownListeners();
                        this._logFileStream.on('error', err => {
                            handleError(err);
                        });
                    }
                    catch (err) {
                        handleError(err);
                    }
                }
            }
        });
    }
    logDateTime() {
        let d = new Date();
        let dateString = d.getUTCFullYear() + '-' + `${d.getUTCMonth() + 1}` + '-' + d.getUTCDate();
        const timeAndDateStamp = dateString + ', ' + getFormattedTimeString();
        this.log(timeAndDateStamp + '\n', logger_1.LogLevel.Verbose, false);
    }
    setupShutdownListeners() {
        process.addListener('beforeExit', this.beforeExitCallback);
        process.addListener('SIGTERM', this.disposeCallback);
        process.addListener('SIGINT', this.disposeCallback);
    }
    removeShutdownListeners() {
        process.removeListener('beforeExit', this.beforeExitCallback);
        process.removeListener('SIGTERM', this.disposeCallback);
        process.removeListener('SIGINT', this.disposeCallback);
    }
    dispose() {
        return new Promise(resolve => {
            this.removeShutdownListeners();
            if (this._logFileStream) {
                this._logFileStream.end(resolve);
                this._logFileStream = null;
            }
            else {
                resolve();
            }
        });
    }
    log(msg, level, prependTimestamp = true) {
        if (this._minLogLevel === logger_1.LogLevel.Stop) {
            return;
        }
        if (level >= this._minLogLevel) {
            this.sendLog(msg, level);
        }
        if (this._logToConsole) {
            const logFn = level === logger_1.LogLevel.Error ? console.error :
                level === logger_1.LogLevel.Warn ? console.warn :
                    null;
            if (logFn) {
                logFn(logger_1.trimLastNewline(msg));
            }
        }
        // If an error, prepend with '[Error]'
        if (level === logger_1.LogLevel.Error) {
            msg = `[${logger_1.LogLevel[level]}] ${msg}`;
        }
        if (this._prependTimestamp && prependTimestamp) {
            msg = '[' + getFormattedTimeString() + '] ' + msg;
        }
        if (this._logFileStream) {
            this._logFileStream.write(msg);
        }
    }
    sendLog(msg, level) {
        // Truncate long messages, they can hang VS Code
        if (msg.length > 1500) {
            const endsInNewline = !!msg.match(/(\n|\r\n)$/);
            msg = msg.substr(0, 1500) + '[...]';
            if (endsInNewline) {
                msg = msg + '\n';
            }
        }
        if (this._logCallback) {
            const event = new logger_1.LogOutputEvent(msg, level);
            this._logCallback(event);
        }
    }
}
exports.InternalLogger = InternalLogger;
function getFormattedTimeString() {
    let d = new Date();
    let hourString = _padZeroes(2, String(d.getUTCHours()));
    let minuteString = _padZeroes(2, String(d.getUTCMinutes()));
    let secondString = _padZeroes(2, String(d.getUTCSeconds()));
    let millisecondString = _padZeroes(3, String(d.getUTCMilliseconds()));
    return hourString + ':' + minuteString + ':' + secondString + '.' + millisecondString + ' UTC';
}
function _padZeroes(minDesiredLength, numberToPad) {
    if (numberToPad.length >= minDesiredLength) {
        return numberToPad;
    }
    else {
        return String('0'.repeat(minDesiredLength) + numberToPad).slice(-minDesiredLength);
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW50ZXJuYWxMb2dnZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9zcmMvaW50ZXJuYWxMb2dnZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Z0dBR2dHOzs7Ozs7Ozs7Ozs7QUFFaEcseUJBQXlCO0FBQ3pCLDZCQUE2QjtBQUM3QixpQ0FBaUM7QUFFakMsU0FBUyxhQUFhLENBQUMsTUFBYztJQUNwQyxPQUFPLElBQUksT0FBTyxDQUFDLENBQUMsT0FBTyxFQUFFLE1BQU0sRUFBRSxFQUFFO1FBQ3RDLE1BQU0sQ0FBQyxNQUFNLEVBQUUsQ0FBQyxHQUFHLEVBQUUsSUFBSSxFQUFFLEVBQUU7WUFDNUIsSUFBSSxHQUFHLEVBQUU7Z0JBQ1IsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDO2FBQ1o7aUJBQU07Z0JBQ04sT0FBTyxDQUFDLElBQUksQ0FBQyxDQUFDO2FBQ2Q7UUFDRixDQUFDLENBQUMsQ0FBQztJQUNKLENBQUMsQ0FBQyxDQUFDO0FBQ0osQ0FBQztBQUVELHFDQUE0SDtBQUU1SDs7O0dBR0c7QUFDSCxNQUFhLGNBQWM7SUFtQjFCLFlBQVksV0FBeUIsRUFBRSxRQUFrQjtRQVR6RCxrREFBa0Q7UUFDMUMsdUJBQWtCLEdBQUcsR0FBRyxFQUFFLENBQUMsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO1FBU2pELElBQUksQ0FBQyxZQUFZLEdBQUcsV0FBVyxDQUFDO1FBQ2hDLElBQUksQ0FBQyxhQUFhLEdBQUcsUUFBUSxDQUFDO1FBRTlCLElBQUksQ0FBQyxZQUFZLEdBQUcsaUJBQVEsQ0FBQyxJQUFJLENBQUM7UUFFbEMsSUFBSSxDQUFDLGVBQWUsR0FBRyxDQUFDLE1BQWMsRUFBRSxJQUFZLEVBQUUsRUFBRTtZQUN2RCxJQUFJLENBQUMsT0FBTyxFQUFFLENBQUM7WUFFZiw0Q0FBNEM7WUFDNUMseURBQXlEO1lBQ3pELElBQUksR0FBRyxJQUFJLElBQUksQ0FBQyxDQUFDLENBQUMsU0FBUztZQUMzQixJQUFJLElB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WUFBWSxHQUFHLFVBQVUsQ0FBQyxDQUFDLEVBQUUsTUFBTSxDQUFDLENBQUMsQ0FBQyxhQUFhLEVBQUUsQ0FBQyxDQUFDLENBQUM7SUFDNUQsSUFBSSxZQUFZLEdBQUcsVUFBVSxDQUFDLENBQUMsRUFBRSxNQUFNLENBQUMsQ0FBQyxDQUFDLGFBQWEsRUFBRSxDQUFDLENBQUMsQ0FBQztJQUM1RCxJQUFJLGlCQUFpQixHQUFHLFVBQVUsQ0FBQyxDQUFDLEVBQUUsTUFBTSxDQUFDLENBQUMsQ0FBQyxrQkFBa0IsRUFBRSxDQUFDLENBQUMsQ0FBQztJQUN0RSxPQUFPLFVBQVUsR0FBRyxHQUFHLEdBQUcsWUFBWSxHQUFHLEdBQUcsR0FBRyxZQUFZLEdBQUcsR0FBRyxHQUFHLGlCQUFpQixHQUFHLE1BQU0sQ0FBQztBQUNoRyxDQUFDO0FBRUQsU0FBUyxVQUFVLENBQUMsZ0JBQXdCLEVBQUUsV0FBbUI7SUFDaEUsSUFBSSxXQUFXLENBQUMsTUFBTSxJQUFJLGdCQUFnQixFQUFFO1FBQzNDLE9BQU8sV0FBVyxDQUFDO0tBQ25CO1NBQU07UUFDTixPQUFPLE1BQU0sQ0FBQyxHQUFHLENBQUMsTUFBTSxDQUFDLGdCQUFnQixDQUFDLEdBQUcsV0FBVyxDQUFDLENBQUMsS0FBSyxDQUFDLENBQUMsZ0JBQWdCLENBQUMsQ0FBQztLQUNuRjtBQUNGLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuICogIENvcHlyaWdodCAoYykgTWljcm9zb2Z0IENvcnBvcmF0aW9uLiBBbGwgcmlnaHRzIHJlc2VydmVkLlxuICogIExpY2Vuc2VkIHVuZGVyIHRoZSBNSVQgTGljZW5zZS4gU2VlIExpY2Vuc2UudHh0IGluIHRoZSBwcm9qZWN0IHJvb3QgZm9yIGxpY2Vuc2UgaW5mb3JtYXRpb24uXG4gKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tKi9cblxuaW1wb3J0ICogYXMgZnMgZnJvbSAnZnMnO1xuaW1wb3J0ICogYXMgcGF0aCBmcm9tICdwYXRoJztcbmltcG9ydCAqIGFzIG1rZGlycCBmcm9tICdta2RpcnAnO1xuXG5mdW5jdGlvbiBta2RpcnBQcm9taXNlKGZvbGRlcjogc3RyaW5nKTogUHJvbWlzZTxzdHJpbmc+IHtcblx0cmV0dXJuIG5ldyBQcm9taXNlKChyZXNvbHZlLCByZWplY3QpID0+IHtcblx0XHRta2RpcnAoZm9sZGVyLCAoZXJyLCBtYWRlKSA9PiB7XG5cdFx0XHRpZiAoZXJyKSB7XG5cdFx0XHRcdHJlamVjdChlcnIpO1xuXHRcdFx0fSBlbHNlIHtcblx0XHRcdFx0cmVzb2x2ZShtYWRlKTtcblx0XHRcdH1cblx0XHR9KTtcblx0fSk7XG59XG5cbmltcG9ydCB7IExvZ0xldmVsLCBJTG9nQ2FsbGJhY2ssIHRyaW1MYXN0TmV3bGluZSwgTG9nT3V0cHV0RXZlbnQsIElJbnRlcm5hbExvZ2dlck9wdGlvbnMsIElJbnRlcm5hbExvZ2dlciB9IGZyb20gJy4vbG9nZ2VyJztcblxuLyoqXG4gKiBNYW5hZ2VzIGxvZ2dpbmcsIHdoZXRoZXIgdG8gY29uc29sZS5sb2csIGZpbGUsIG9yIFZTIENvZGUgY29uc29sZS5cbiAqIEVuY2Fwc3VsYXRlcyB0aGUgc3RhdGUgc3BlY2lmaWMgdG8gZWFjaCBsb2dnaW5nIHNlc3Npb25cbiAqL1xuZXhwb3J0IGNsYXNzIEludGVybmFsTG9nZ2VyIGltcGxlbWVudHMgSUludGVybmFsTG9nZ2VyIHtcblx0cHJpdmF0ZSBfbWluTG9nTGV2ZWw6IExvZ0xldmVsO1xuXHRwcml2YXRlIF9sb2dUb0NvbnNvbGU6IGJvb2xlYW47XG5cblx0LyoqIExvZyBpbmZvIHRoYXQgbWVldHMgbWluTG9nTGV2ZWwgaXMgc2VudCB0byB0aGlzIGNhbGxiYWNrLiAqL1xuXHRwcml2YXRlIF9sb2dDYWxsYmFjazogSUxvZ0NhbGxiYWNrO1xuXG5cdC8qKiBXcml0ZSBzdGVhbSBmb3IgbG9nIGZpbGUgKi9cblx0cHJpdmF0ZSBfbG9nRmlsZVN0cmVhbTogZnMuV3JpdGVTdHJlYW07XG5cblx0LyoqIERpc3Bvc2UgYW5kIGFsbG93IGV4aXQgdG8gY29udGludWUgbm9ybWFsbHkgKi9cblx0cHJpdmF0ZSBiZWZvcmVFeGl0Q2FsbGJhY2sgPSAoKSA9PiB0aGlzLmRpc3Bvc2UoKTtcblxuXHQvKiogRGlzcG9zZSBhbmQgZXhpdCAqL1xuXHRwcml2YXRlIGRpc3Bvc2VDYWxsYmFjaztcblxuXHQvKiogV2hldGhlciB0byBhZGQgYSB0aW1lc3RhbXAgdG8gbWVzc2FnZXMgaW4gdGhlIGxvZ2ZpbGUgKi9cblx0cHJpdmF0ZSBfcHJlcGVuZFRpbWVzdGFtcDogYm9vbGVhbjtcblxuXHRjb25zdHJ1Y3Rvcihsb2dDYWxsYmFjazogSUxvZ0NhbGxiYWNrLCBpc1NlcnZlcj86IGJvb2xlYW4pIHtcblx0XHR0aGlzLl9sb2dDYWxsYmFjayA9IGxvZ0NhbGxiYWNrO1xuXHRcdHRoaXMuX2xvZ1RvQ29uc29sZSA9IGlzU2VydmVyO1xuXG5cdFx0dGhpcy5fbWluTG9nTGV2ZWwgPSBMb2dMZXZlbC5XYXJuO1xuXG5cdFx0dGhpcy5kaXNwb3NlQ2FsbGJhY2sgPSAoc2lnbmFsOiBzdHJpbmcsIGNvZGU6IG51bWJlcikgPT4ge1xuXHRcdFx0dGhpcy5kaXNwb3NlKCk7XG5cblx0XHRcdC8vIEV4aXQgd2l0aCAxMjggKyB2YWx1ZSBvZiB0aGUgc2lnbmFsIGNvZGUuXG5cdFx0XHQvLyBodHRwczovL25vZGVqcy5vcmcvYXBpL3Byb2Nlc3MuaHRtbCNwcm9jZXNzX2V4aXRfY29kZXNcblx0XHRcdGNvZGUgPSBjb2RlIHx8IDI7IC8vIFNJR0lOVFxuXHRcdFx0Y29kZSArPSAxMjg7XG5cblx0XHRcdHByb2Nlc3MuZXhpdChjb2RlKTtcblx0XHR9O1xuXHR9XG5cblx0cHVibGljIGFzeW5jIHNldHVwKG9wdGlvbnM6IElJbnRlcm5hbExvZ2dlck9wdGlvbnMpOiBQcm9taXNlPHZvaWQ+IHtcblx0XHR0aGlzLl9taW5Mb2dMZXZlbCA9IG9wdGlvbnMuY29uc29sZU1pbkxvZ0xldmVsO1xuXHRcdHRoaXMuX3ByZXBlbmRUaW1lc3RhbXAgPSBvcHRpb25zLnByZXBlbmRUaW1lc3RhbXA7XG5cblx0XHQvLyBPcGVuIGEgbG9nIGZpbGUgaW4gdGhlIHNwZWNpZmllZCBsb2NhdGlvbi4gT3ZlcndyaXR0ZW4gb24gZWFjaCBydW4uXG5cdFx0aWYgKG9wdGlvbnMubG9nRmlsZVBhdGgpIHtcblx0XHRcdGlmICghcGF0aC5pc0Fic29sdXRlKG9wdGlvbnMubG9nRmlsZVBhdGgpKSB7XG5cdFx0XHRcdHRoaXMubG9nKGBsb2dGaWxlUGF0aCBtdXN0IGJlIGFuIGFic29sdXRlIHBhdGg6ICR7b3B0aW9ucy5sb2dGaWxlUGF0aH1gLCBMb2dMZXZlbC5FcnJvcik7XG5cdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRjb25zdCBoYW5kbGVFcnJvciA9IGVyciA9PiB0aGlzLnNlbmRMb2coYEVycm9yIGNyZWF0aW5nIGxvZyBmaWxlIGF0IHBhdGg6ICR7b3B0aW9ucy5sb2dGaWxlUGF0aH0uIEVycm9yOiAke2Vyci50b1N0cmluZygpfVxcbmAsIExvZ0xldmVsLkVycm9yKTtcblxuXHRcdFx0XHR0cnkge1xuXHRcdFx0XHRcdGF3YWl0IG1rZGlycFByb21pc2UocGF0aC5kaXJuYW1lKG9wdGlvbnMubG9nRmlsZVBhdGgpKTtcblx0XHRcdFx0XHR0aGlzLmxvZyhgVmVyYm9zZSBsb2dzIGFyZSB3cml0dGVuIHRvOlxcbmAsIExvZ0xldmVsLldhcm4pO1xuXHRcdFx0XHRcdHRoaXMubG9nKG9wdGlvbnMubG9nRmlsZVBhdGggKyAnXFxuJywgTG9nTGV2ZWwuV2Fybik7XG5cblx0XHRcdFx0XHR0aGlzLl9sb2dGaWxlU3RyZWFtID0gZnMuY3JlYXRlV3JpdGVTdHJlYW0ob3B0aW9ucy5sb2dGaWxlUGF0aCk7XG5cdFx0XHRcdFx0dGhpcy5sb2dEYXRlVGltZSgpO1xuXHRcdFx0XHRcdHRoaXMuc2V0dXBTaHV0ZG93bkxpc3RlbmVycygpO1xuXHRcdFx0XHRcdHRoaXMuX2xvZ0ZpbGVTdHJlYW0ub24oJ2Vycm9yJywgZXJyID0+IHtcblx0XHRcdFx0XHRcdGhhbmRsZUVycm9yKGVycik7XG5cdFx0XHRcdFx0fSk7XG5cdFx0XHRcdH0gY2F0Y2ggKGVycikge1xuXHRcdFx0XHRcdGhhbmRsZUVycm9yKGVycik7XG5cdFx0XHRcdH1cblx0XHRcdH1cblx0XHR9XG5cdH1cblxuXHRwcml2YXRlIGxvZ0RhdGVUaW1lKCk6IHZvaWQge1xuXHRcdGxldCBkID0gbmV3IERhdGUoKTtcblx0XHRsZXQgZGF0ZVN0cmluZyA9IGQuZ2V0VVRDRnVsbFllYXIoKSArICctJyArIGAke2QuZ2V0VVRDTW9udGgoKSArIDF9YCArICctJyArIGQuZ2V0VVRDRGF0ZSgpO1xuXHRcdGNvbnN0IHRpbWVBbmREYXRlU3RhbXAgPSBkYXRlU3RyaW5nICsgJywgJyArIGdldEZvcm1hdHRlZFRpbWVTdHJpbmcoKTtcblx0XHR0aGlzLmxvZyh0aW1lQW5kRGF0ZVN0YW1wICsgJ1xcbicsIExvZ0xldmVsLlZlcmJvc2UsIGZhbHNlKTtcblx0fVxuXG5cdHByaXZhdGUgc2V0dXBTaHV0ZG93bkxpc3RlbmVycygpOiB2b2lkIHtcblx0XHRwcm9jZXNzLmFkZExpc3RlbmVyKCdiZWZvcmVFeGl0JywgdGhpcy5iZWZvcmVFeGl0Q2FsbGJhY2spO1xuXHRcdHByb2Nlc3MuYWRkTGlzdGVuZXIoJ1NJR1RFUk0nLCB0aGlzLmRpc3Bvc2VDYWxsYmFjayk7XG5cdFx0cHJvY2Vzcy5hZGRMaXN0ZW5lcignU0lHSU5UJywgdGhpcy5kaXNwb3NlQ2FsbGJhY2spO1xuXHR9XG5cblx0cHJpdmF0ZSByZW1vdmVTaHV0ZG93bkxpc3RlbmVycygpOiB2b2lkIHtcblx0XHRwcm9jZXNzLnJlbW92ZUxpc3RlbmVyKCdiZWZvcmVFeGl0JywgdGhpcy5iZWZvcmVFeGl0Q2FsbGJhY2spO1xuXHRcdHByb2Nlc3MucmVtb3ZlTGlzdGVuZXIoJ1NJR1RFUk0nLCB0aGlzLmRpc3Bvc2VDYWxsYmFjayk7XG5cdFx0cHJvY2Vzcy5yZW1vdmVMaXN0ZW5lcignU0lHSU5UJywgdGhpcy5kaXNwb3NlQ2FsbGJhY2spO1xuXHR9XG5cblx0cHVibGljIGRpc3Bvc2UoKTogUHJvbWlzZTx2b2lkPiB7XG5cdFx0cmV0dXJuIG5ldyBQcm9taXNlKHJlc29sdmUgPT4ge1xuXHRcdFx0dGhpcy5yZW1vdmVTaHV0ZG93bkxpc3RlbmVycygpO1xuXHRcdFx0aWYgKHRoaXMuX2xvZ0ZpbGVTdHJlYW0pIHtcblx0XHRcdFx0dGhpcy5fbG9nRmlsZVN0cmVhbS5lbmQocmVzb2x2ZSk7XG5cdFx0XHRcdHRoaXMuX2xvZ0ZpbGVTdHJlYW0gPSBudWxsO1xuXHRcdFx0fSBlbHNlIHtcblx0XHRcdFx0cmVzb2x2ZSgpO1xuXHRcdFx0fVxuXHRcdH0pO1xuXHR9XG5cblx0cHVibGljIGxvZyhtc2c6IHN0cmluZywgbGV2ZWw6IExvZ0xldmVsLCBwcmVwZW5kVGltZXN0YW1wID0gdHJ1ZSk6IHZvaWQge1xuXHRcdGlmICh0aGlzLl9taW5Mb2dMZXZlbCA9PT0gTG9nTGV2ZWwuU3RvcCkge1xuXHRcdFx0cmV0dXJuO1xuXHRcdH1cblxuXHRcdGlmIChsZXZlbCA+PSB0aGlzLl9taW5Mb2dMZXZlbCkge1xuXHRcdFx0dGhpcy5zZW5kTG9nKG1zZywgbGV2ZWwpO1xuXHRcdH1cblxuXHRcdGlmICh0aGlzLl9sb2dUb0NvbnNvbGUpIHtcblx0XHRcdGNvbnN0IGxvZ0ZuID1cblx0XHRcdFx0bGV2ZWwgPT09IExvZ0xldmVsLkVycm9yID8gY29uc29sZS5lcnJvciA6XG5cdFx0XHRcdGxldmVsID09PSBMb2dMZXZlbC5XYXJuID8gY29uc29sZS53YXJuIDpcblx0XHRcdFx0bnVsbDtcblxuXHRcdFx0aWYgKGxvZ0ZuKSB7XG5cdFx0XHRcdGxvZ0ZuKHRyaW1MYXN0TmV3bGluZShtc2cpKTtcblx0XHRcdH1cblx0XHR9XG5cblx0XHQvLyBJZiBhbiBlcnJvciwgcHJlcGVuZCB3aXRoICdbRXJyb3JdJ1xuXHRcdGlmIChsZXZlbCA9PT0gTG9nTGV2ZWwuRXJyb3IpIHtcblx0XHRcdG1zZyA9IGBbJHtMb2dMZXZlbFtsZXZlbF19XSAke21zZ31gO1xuXHRcdH1cblxuXHRcdGlmICh0aGlzLl9wcmVwZW5kVGltZXN0YW1wICYmIHByZXBlbmRUaW1lc3RhbXApIHtcblx0XHRcdG1zZyA9ICdbJyArIGdldEZvcm1hdHRlZFRpbWVTdHJpbmcoKSArICddICcgKyBtc2c7XG5cdFx0fVxuXG5cdFx0aWYgKHRoaXMuX2xvZ0ZpbGVTdHJlYW0pIHtcblx0XHRcdHRoaXMuX2xvZ0ZpbGVTdHJlYW0ud3JpdGUobXNnKTtcblx0XHR9XG5cdH1cblxuXHRwcml2YXRlIHNlbmRMb2cobXNnOiBzdHJpbmcsIGxldmVsOiBMb2dMZXZlbCk6IHZvaWQge1xuXHRcdC8vIFRydW5jYXRlIGxvbmcgbWVzc2FnZXMsIHRoZXkgY2FuIGhhbmcgVlMgQ29kZVxuXHRcdGlmIChtc2cubGVuZ3RoID4gMTUwMCkge1xuXHRcdFx0Y29uc3QgZW5kc0luTmV3bGluZSA9ICEhbXNnLm1hdGNoKC8oXFxufFxcclxcbikkLyk7XG5cdFx0XHRtc2cgPSBtc2cuc3Vic3RyKDAsIDE1MDApICsgJ1suLi5dJztcblx0XHRcdGlmIChlbmRzSW5OZXdsaW5lKSB7XG5cdFx0XHRcdG1zZyA9IG1zZyArICdcXG4nO1xuXHRcdFx0fVxuXHRcdH1cblxuXHRcdGlmICh0aGlzLl9sb2dDYWxsYmFjaykge1xuXHRcdFx0Y29uc3QgZXZlbnQgPSBuZXcgTG9nT3V0cHV0RXZlbnQobXNnLCBsZXZlbCk7XG5cdFx0XHR0aGlzLl9sb2dDYWxsYmFjayhldmVudCk7XG5cdFx0fVxuXHR9XG59XG5cbmZ1bmN0aW9uIGdldEZvcm1hdHRlZFRpbWVTdHJpbmcoKTogc3RyaW5nIHtcblx0bGV0IGQgPSBuZXcgRGF0ZSgpO1xuXHRsZXQgaG91clN0cmluZyA9IF9wYWRaZXJvZXMoMiwgU3RyaW5nKGQuZ2V0VVRDSG91cnMoKSkpO1xuXHRsZXQgbWludXRlU3RyaW5nID0gX3BhZFplcm9lcygyLCBTdHJpbmcoZC5nZXRVVENNaW51dGVzKCkpKTtcblx0bGV0IHNlY29uZFN0cmluZyA9IF9wYWRaZXJvZXMoMiwgU3RyaW5nKGQuZ2V0VVRDU2Vjb25kcygpKSk7XG5cdGxldCBtaWxsaXNlY29uZFN0cmluZyA9IF9wYWRaZXJvZXMoMywgU3RyaW5nKGQuZ2V0VVRDTWlsbGlzZWNvbmRzKCkpKTtcblx0cmV0dXJuIGhvdXJTdHJpbmcgKyAnOicgKyBtaW51dGVTdHJpbmcgKyAnOicgKyBzZWNvbmRTdHJpbmcgKyAnLicgKyBtaWxsaXNlY29uZFN0cmluZyArICcgVVRDJztcbn1cblxuZnVuY3Rpb24gX3BhZFplcm9lcyhtaW5EZXNpcmVkTGVuZ3RoOiBudW1iZXIsIG51bWJlclRvUGFkOiBzdHJpbmcpOiBzdHJpbmcge1xuXHRpZiAobnVtYmVyVG9QYWQubGVuZ3RoID49IG1pbkRlc2lyZWRMZW5ndGgpIHtcblx0XHRyZXR1cm4gbnVtYmVyVG9QYWQ7XG5cdH0gZWxzZSB7XG5cdFx0cmV0dXJuIFN0cmluZygnMCcucmVwZWF0KG1pbkRlc2lyZWRMZW5ndGgpICsgbnVtYmVyVG9QYWQpLnNsaWNlKC1taW5EZXNpcmVkTGVuZ3RoKTtcblx0fVxufVxuIl19

/***/ }),

/***/ 136:
/***/ (function(module, exports, __webpack_require__) {

var path = __webpack_require__(2);
var fs = __webpack_require__(6);
var _0777 = parseInt('0777', 8);

module.exports = mkdirP.mkdirp = mkdirP.mkdirP = mkdirP;

function mkdirP (p, opts, f, made) {
    if (typeof opts === 'function') {
        f = opts;
        opts = {};
    }
    else if (!opts || typeof opts !== 'object') {
        opts = { mode: opts };
    }
    
    var mode = opts.mode;
    var xfs = opts.fs || fs;
    
    if (mode === undefined) {
        mode = _0777
    }
    if (!made) made = null;
    
    var cb = f || function () {};
    p = path.resolve(p);
    
    xfs.mkdir(p, mode, function (er) {
        if (!er) {
            made = made || p;
            return cb(null, made);
        }
        switch (er.code) {
            case 'ENOENT':
                if (path.dirname(p) === p) return cb(er);
                mkdirP(path.dirname(p), opts, function (er, made) {
                    if (er) cb(er, made);
                    else mkdirP(p, opts, cb, made);
                });
                break;

            // In the case of any other error, just see if there's a dir
            // there already.  If so, then hooray!  If not, then something
            // is borked.
            default:
                xfs.stat(p, function (er2, stat) {
                    // if the stat fails, then that's super weird.
                    // let the original error be the failure reason.
                    if (er2 || !stat.isDirectory()) cb(er, made)
                    else cb(null, made);
                });
                break;
        }
    });
}

mkdirP.sync = function sync (p, opts, made) {
    if (!opts || typeof opts !== 'object') {
        opts = { mode: opts };
    }
    
    var mode = opts.mode;
    var xfs = opts.fs || fs;
    
    if (mode === undefined) {
        mode = _0777
    }
    if (!made) made = null;

    p = path.resolve(p);

    try {
        xfs.mkdirSync(p, mode);
        made = made || p;
    }
    catch (err0) {
        switch (err0.code) {
            case 'ENOENT' :
                made = sync(path.dirname(p), opts, made);
                sync(p, opts, made);
                break;

            // In the case of any other error, just see if there's a dir
            // there already.  If so, then hooray!  If not, then something
            // is borked.
            default:
                var stat;
                try {
                    stat = xfs.statSync(p);
                }
                catch (err1) {
                    throw err0;
                }
                if (!stat.isDirectory()) throw err0;
                break;
        }
    }

    return made;
};


/***/ }),

/***/ 137:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.Handles = void 0;
class Handles {
    constructor(startHandle) {
        this.START_HANDLE = 1000;
        this._handleMap = new Map();
        this._nextHandle = typeof startHandle === 'number' ? startHandle : this.START_HANDLE;
    }
    reset() {
        this._nextHandle = this.START_HANDLE;
        this._handleMap = new Map();
    }
    create(value) {
        var handle = this._nextHandle++;
        this._handleMap.set(handle, value);
        return handle;
    }
    get(handle, dflt) {
        return this._handleMap.get(handle) || dflt;
    }
}
exports.Handles = Handles;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaGFuZGxlcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uL3NyYy9oYW5kbGVzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQTs7O2dHQUdnRzs7O0FBRWhHLE1BQWEsT0FBTztJQU9uQixZQUFtQixXQUFvQjtRQUwvQixpQkFBWSxHQUFHLElBQUksQ0FBQztRQUdwQixlQUFVLEdBQUcsSUFBSSxHQUFHLEVBQWEsQ0FBQztRQUd6QyxJQUFJLENBQUMsV0FBVyxHQUFHLE9BQU8sV0FBVyxLQUFLLFFBQVEsQ0FBQyxDQUFDLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsWUFBWSxDQUFDO0lBQ3RGLENBQUM7SUFFTSxLQUFLO1FBQ1gsSUFBSSxDQUFDLFdBQVcsR0FBRyxJQUFJLENBQUMsWUFBWSxDQUFDO1FBQ3JDLElBQUksQ0FBQyxVQUFVLEdBQUcsSUFBSSxHQUFHLEVBQWEsQ0FBQztJQUN4QyxDQUFDO0lBRU0sTUFBTSxDQUFDLEtBQVE7UUFDckIsSUFBSSxNQUFNLEdBQUcsSUFBSSxDQUFDLFdBQVcsRUFBRSxDQUFDO1FBQ2hDLElBQUksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLE1BQU0sRUFBRSxLQUFLLENBQUMsQ0FBQztRQUNuQyxPQUFPLE1BQU0sQ0FBQztJQUNmLENBQUM7SUFFTSxHQUFHLENBQUMsTUFBYyxFQUFFLElBQVE7UUFDbEMsT0FBTyxJQUFJLENBQUMsVUFBVSxDQUFDLEdBQUcsQ0FBQyxNQUFNLENBQUMsSUFBSSxJQUFJLENBQUM7SUFDNUMsQ0FBQztDQUNEO0FBekJELDBCQXlCQyIsInNvdXJjZXNDb250ZW50IjpbIi8qLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4gKiAgQ29weXJpZ2h0IChjKSBNaWNyb3NvZnQgQ29ycG9yYXRpb24uIEFsbCByaWdodHMgcmVzZXJ2ZWQuXG4gKiAgTGljZW5zZWQgdW5kZXIgdGhlIE1JVCBMaWNlbnNlLiBTZWUgTGljZW5zZS50eHQgaW4gdGhlIHByb2plY3Qgcm9vdCBmb3IgbGljZW5zZSBpbmZvcm1hdGlvbi5cbiAqLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0qL1xuXG5leHBvcnQgY2xhc3MgSGFuZGxlczxUPiB7XG5cblx0cHJpdmF0ZSBTVEFSVF9IQU5ETEUgPSAxMDAwO1xuXG5cdHByaXZhdGUgX25leHRIYW5kbGUgOiBudW1iZXI7XG5cdHByaXZhdGUgX2hhbmRsZU1hcCA9IG5ldyBNYXA8bnVtYmVyLCBUPigpO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihzdGFydEhhbmRsZT86IG51bWJlcikge1xuXHRcdHRoaXMuX25leHRIYW5kbGUgPSB0eXBlb2Ygc3RhcnRIYW5kbGUgPT09ICdudW1iZXInID8gc3RhcnRIYW5kbGUgOiB0aGlzLlNUQVJUX0hBTkRMRTtcblx0fVxuXG5cdHB1YmxpYyByZXNldCgpOiB2b2lkIHtcblx0XHR0aGlzLl9uZXh0SGFuZGxlID0gdGhpcy5TVEFSVF9IQU5ETEU7XG5cdFx0dGhpcy5faGFuZGxlTWFwID0gbmV3IE1hcDxudW1iZXIsIFQ+KCk7XG5cdH1cblxuXHRwdWJsaWMgY3JlYXRlKHZhbHVlOiBUKTogbnVtYmVyIHtcblx0XHR2YXIgaGFuZGxlID0gdGhpcy5fbmV4dEhhbmRsZSsrO1xuXHRcdHRoaXMuX2hhbmRsZU1hcC5zZXQoaGFuZGxlLCB2YWx1ZSk7XG5cdFx0cmV0dXJuIGhhbmRsZTtcblx0fVxuXG5cdHB1YmxpYyBnZXQoaGFuZGxlOiBudW1iZXIsIGRmbHQ/OiBUKTogVCB7XG5cdFx0cmV0dXJuIHRoaXMuX2hhbmRsZU1hcC5nZXQoaGFuZGxlKSB8fCBkZmx0O1xuXHR9XG59XG4iXX0=

/***/ }),

/***/ 2:
/***/ (function(module, exports) {

module.exports = require("path");

/***/ }),

/***/ 20:
/***/ (function(module, exports) {

module.exports = require("url");

/***/ }),

/***/ 24:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------
 * Copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.killProcess = exports.killProcessTree = void 0;
const kill = __webpack_require__(73);
// Kill a process and its children, returning a promise.
function killProcessTree(p, logger) {
    if (!logger) {
        logger = console.log;
    }
    if (!p || !p.pid || p.exitCode !== null) {
        return Promise.resolve();
    }
    return new Promise((resolve) => {
        kill(p.pid, (err) => {
            if (err) {
                logger(`Error killing process ${p.pid}: ${err}`);
            }
            resolve();
        });
    });
}
exports.killProcessTree = killProcessTree;
// Kill a process.
//
// READ THIS BEFORE USING THE FUNCTION:
//
// TODO: This function is kept for historical reasons and should be removed once
// its user (go-outline) is replaced. Outlining uses this function and not
// killProcessTree because of performance issues that were observed in the past.
// See https://go-review.googlesource.com/c/vscode-go/+/242518/ for more
// details and background.
function killProcess(p) {
    if (p && p.pid && p.exitCode === null) {
        try {
            p.kill();
        }
        catch (e) {
            console.log(`Error killing process ${p.pid}: ${e}`);
        }
    }
}
exports.killProcess = killProcess;


/***/ }),

/***/ 26:
/***/ (function(module, exports) {

module.exports = require("net");

/***/ }),

/***/ 29:
/***/ (function(module, exports) {

module.exports = require("os");

/***/ }),

/***/ 3:
/***/ (function(module, exports) {

module.exports = require("util");

/***/ }),

/***/ 33:
/***/ (function(module, exports) {

module.exports = require("events");

/***/ }),

/***/ 4:
/***/ (function(module, exports) {

module.exports = require("child_process");

/***/ }),

/***/ 42:
/***/ (function(module, exports, __webpack_require__) {

"use strict";
/*---------------------------------------------------------
 * Copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/

Object.defineProperty(exports, "__esModule", { value: true });
exports.logInfo = exports.logError = exports.logVerbose = exports.setLogConfig = void 0;
// Our log level.
var LogLevel;
(function (LogLevel) {
    LogLevel[LogLevel["Off"] = 100] = "Off";
    LogLevel[LogLevel["Error"] = 50] = "Error";
    LogLevel[LogLevel["Info"] = 30] = "Info";
    LogLevel[LogLevel["Verbose"] = 20] = "Verbose";
    // TODO: Trace, Warn level
})(LogLevel || (LogLevel = {}));
let currentLogLevel = LogLevel.Error;
const levelMap = {
    off: LogLevel.Off,
    error: LogLevel.Error,
    info: LogLevel.Info,
    verbose: LogLevel.Verbose,
};
function levelPrefix(l) {
    switch (l) {
        case LogLevel.Off: return 'Go[O]:';
        case LogLevel.Error: return 'Go[E]:';
        case LogLevel.Info: return 'Go[I]:';
        case LogLevel.Verbose: return 'Go[V]:';
        default: return 'Go[?]:';
    }
}
function setLogConfig(cfg) {
    const logLevel = (cfg === null || cfg === void 0 ? void 0 : cfg.level) || 'error';
    const l = levelMap[logLevel];
    if (l) {
        currentLogLevel = l;
        return;
    }
    logError(`setLogLevel requested with invalid log level ${logLevel}, ignoring...`);
}
exports.setLogConfig = setLogConfig;
// tslint:disable-next-line:no-any
function log(logLevel, ...args) {
    if (logLevel < currentLogLevel) {
        return;
    }
    const p = levelPrefix(logLevel);
    const a = Array.from(args);
    a.unshift(p);
    console.log(...a);
    // TODO: support logging in vscode output channel.
}
// tslint:disable-next-line:no-any
function logVerbose(...args) {
    log(LogLevel.Verbose, ...args);
}
exports.logVerbose = logVerbose;
// tslint:disable-next-line:no-any
function logError(...args) {
    log(LogLevel.Error, ...args);
}
exports.logError = logError;
// tslint:disable-next-line:no-any
function logInfo(...args) {
    log(LogLevel.Info, ...args);
}
exports.logInfo = logInfo;


/***/ }),

/***/ 46:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.DebugSession = exports.ErrorDestination = exports.InvalidatedEvent = exports.ProgressEndEvent = exports.ProgressUpdateEvent = exports.ProgressStartEvent = exports.CapabilitiesEvent = exports.LoadedSourceEvent = exports.ModuleEvent = exports.BreakpointEvent = exports.ThreadEvent = exports.OutputEvent = exports.TerminatedEvent = exports.InitializedEvent = exports.ContinuedEvent = exports.StoppedEvent = exports.CompletionItem = exports.Module = exports.Breakpoint = exports.Variable = exports.Thread = exports.StackFrame = exports.Scope = exports.Source = void 0;
const protocol_1 = __webpack_require__(132);
const messages_1 = __webpack_require__(47);
const runDebugAdapter_1 = __webpack_require__(133);
const url_1 = __webpack_require__(20);
class Source {
    constructor(name, path, id = 0, origin, data) {
        this.name = name;
        this.path = path;
        this.sourceReference = id;
        if (origin) {
            this.origin = origin;
        }
        if (data) {
            this.adapterData = data;
        }
    }
}
exports.Source = Source;
class Scope {
    constructor(name, reference, expensive = false) {
        this.name = name;
        this.variablesReference = reference;
        this.expensive = expensive;
    }
}
exports.Scope = Scope;
class StackFrame {
    constructor(i, nm, src, ln = 0, col = 0) {
        this.id = i;
        this.source = src;
        this.line = ln;
        this.column = col;
        this.name = nm;
    }
}
exports.StackFrame = StackFrame;
class Thread {
    constructor(id, name) {
        this.id = id;
        if (name) {
            this.name = name;
        }
        else {
            this.name = 'Thread #' + id;
        }
    }
}
exports.Thread = Thread;
class Variable {
    constructor(name, value, ref = 0, indexedVariables, namedVariables) {
        this.name = name;
        this.value = value;
        this.variablesReference = ref;
        if (typeof namedVariables === 'number') {
            this.namedVariables = namedVariables;
        }
        if (typeof indexedVariables === 'number') {
            this.indexedVariables = indexedVariables;
        }
    }
}
exports.Variable = Variable;
class Breakpoint {
    constructor(verified, line, column, source) {
        this.verified = verified;
        const e = this;
        if (typeof line === 'number') {
            e.line = line;
        }
        if (typeof column === 'number') {
            e.column = column;
        }
        if (source) {
            e.source = source;
        }
    }
}
exports.Breakpoint = Breakpoint;
class Module {
    constructor(id, name) {
        this.id = id;
        this.name = name;
    }
}
exports.Module = Module;
class CompletionItem {
    constructor(label, start, length = 0) {
        this.label = label;
        this.start = start;
        this.length = length;
    }
}
exports.CompletionItem = CompletionItem;
class StoppedEvent extends messages_1.Event {
    constructor(reason, threadId, exceptionText) {
        super('stopped');
        this.body = {
            reason: reason
        };
        if (typeof threadId === 'number') {
            this.body.threadId = threadId;
        }
        if (typeof exceptionText === 'string') {
            this.body.text = exceptionText;
        }
    }
}
exports.StoppedEvent = StoppedEvent;
class ContinuedEvent extends messages_1.Event {
    constructor(threadId, allThreadsContinued) {
        super('continued');
        this.body = {
            threadId: threadId
        };
        if (typeof allThreadsContinued === 'boolean') {
            this.body.allThreadsContinued = allThreadsContinued;
        }
    }
}
exports.ContinuedEvent = ContinuedEvent;
class InitializedEvent extends messages_1.Event {
    constructor() {
        super('initialized');
    }
}
exports.InitializedEvent = InitializedEvent;
class TerminatedEvent extends messages_1.Event {
    constructor(restart) {
        super('terminated');
        if (typeof restart === 'boolean' || restart) {
            const e = this;
            e.body = {
                restart: restart
            };
        }
    }
}
exports.TerminatedEvent = TerminatedEvent;
class OutputEvent extends messages_1.Event {
    constructor(output, category = 'console', data) {
        super('output');
        this.body = {
            category: category,
            output: output
        };
        if (data !== undefined) {
            this.body.data = data;
        }
    }
}
exports.OutputEvent = OutputEvent;
class ThreadEvent extends messages_1.Event {
    constructor(reason, threadId) {
        super('thread');
        this.body = {
            reason: reason,
            threadId: threadId
        };
    }
}
exports.ThreadEvent = ThreadEvent;
class BreakpointEvent extends messages_1.Event {
    constructor(reason, breakpoint) {
        super('breakpoint');
        this.body = {
            reason: reason,
            breakpoint: breakpoint
        };
    }
}
exports.BreakpointEvent = BreakpointEvent;
class ModuleEvent extends messages_1.Event {
    constructor(reason, module) {
        super('module');
        this.body = {
            reason: reason,
            module: module
        };
    }
}
exports.ModuleEvent = ModuleEvent;
class LoadedSourceEvent extends messages_1.Event {
    constructor(reason, source) {
        super('loadedSource');
        this.body = {
            reason: reason,
            source: source
        };
    }
}
exports.LoadedSourceEvent = LoadedSourceEvent;
class CapabilitiesEvent extends messages_1.Event {
    constructor(capabilities) {
        super('capabilities');
        this.body = {
            capabilities: capabilities
        };
    }
}
exports.CapabilitiesEvent = CapabilitiesEvent;
class ProgressStartEvent extends messages_1.Event {
    constructor(progressId, title, message) {
        super('progressStart');
        this.body = {
            progressId: progressId,
            title: title
        };
        if (typeof message === 'string') {
            this.body.message = message;
        }
    }
}
exports.ProgressStartEvent = ProgressStartEvent;
class ProgressUpdateEvent extends messages_1.Event {
    constructor(progressId, message) {
        super('progressUpdate');
        this.body = {
            progressId: progressId
        };
        if (typeof message === 'string') {
            this.body.message = message;
        }
    }
}
exports.ProgressUpdateEvent = ProgressUpdateEvent;
class ProgressEndEvent extends messages_1.Event {
    constructor(progressId, message) {
        super('progressEnd');
        this.body = {
            progressId: progressId
        };
        if (typeof message === 'string') {
            this.body.message = message;
        }
    }
}
exports.ProgressEndEvent = ProgressEndEvent;
class InvalidatedEvent extends messages_1.Event {
    constructor(areas, threadId, stackFrameId) {
        super('invalidated');
        this.body = {};
        if (areas) {
            this.body.areas = areas;
        }
        if (threadId) {
            this.body.threadId = threadId;
        }
        if (stackFrameId) {
            this.body.stackFrameId = stackFrameId;
        }
    }
}
exports.InvalidatedEvent = InvalidatedEvent;
var ErrorDestination;
(function (ErrorDestination) {
    ErrorDestination[ErrorDestination["User"] = 1] = "User";
    ErrorDestination[ErrorDestination["Telemetry"] = 2] = "Telemetry";
})(ErrorDestination = exports.ErrorDestination || (exports.ErrorDestination = {}));
;
class DebugSession extends protocol_1.ProtocolServer {
    constructor(obsolete_debuggerLinesAndColumnsStartAt1, obsolete_isServer) {
        super();
        const linesAndColumnsStartAt1 = typeof obsolete_debuggerLinesAndColumnsStartAt1 === 'boolean' ? obsolete_debuggerLinesAndColumnsStartAt1 : false;
        this._debuggerLinesStartAt1 = linesAndColumnsStartAt1;
        this._debuggerColumnsStartAt1 = linesAndColumnsStartAt1;
        this._debuggerPathsAreURIs = false;
        this._clientLinesStartAt1 = true;
        this._clientColumnsStartAt1 = true;
        this._clientPathsAreURIs = false;
        this._isServer = typeof obsolete_isServer === 'boolean' ? obsolete_isServer : false;
        this.on('close', () => {
            this.shutdown();
        });
        this.on('error', (error) => {
            this.shutdown();
        });
    }
    setDebuggerPathFormat(format) {
        this._debuggerPathsAreURIs = format !== 'path';
    }
    setDebuggerLinesStartAt1(enable) {
        this._debuggerLinesStartAt1 = enable;
    }
    setDebuggerColumnsStartAt1(enable) {
        this._debuggerColumnsStartAt1 = enable;
    }
    setRunAsServer(enable) {
        this._isServer = enable;
    }
    /**
     * A virtual constructor...
     */
    static run(debugSession) {
        runDebugAdapter_1.runDebugAdapter(debugSession);
    }
    shutdown() {
        if (this._isServer || this._isRunningInline()) {
            // shutdown ignored in server mode
        }
        else {
            // wait a bit before shutting down
            setTimeout(() => {
                process.exit(0);
            }, 100);
        }
    }
    sendErrorResponse(response, codeOrMessage, format, variables, dest = ErrorDestination.User) {
        let msg;
        if (typeof codeOrMessage === 'number') {
            msg = {
                id: codeOrMessage,
                format: format
            };
            if (variables) {
                msg.variables = variables;
            }
            if (dest & ErrorDestination.User) {
                msg.showUser = true;
            }
            if (dest & ErrorDestination.Telemetry) {
                msg.sendTelemetry = true;
            }
        }
        else {
            msg = codeOrMessage;
        }
        response.success = false;
        response.message = DebugSession.formatPII(msg.format, true, msg.variables);
        if (!response.body) {
            response.body = {};
        }
        response.body.error = msg;
        this.sendResponse(response);
    }
    runInTerminalRequest(args, timeout, cb) {
        this.sendRequest('runInTerminal', args, timeout, cb);
    }
    dispatchRequest(request) {
        const response = new messages_1.Response(request);
        try {
            if (request.command === 'initialize') {
                var args = request.arguments;
                if (typeof args.linesStartAt1 === 'boolean') {
                    this._clientLinesStartAt1 = args.linesStartAt1;
                }
                if (typeof args.columnsStartAt1 === 'boolean') {
                    this._clientColumnsStartAt1 = args.columnsStartAt1;
                }
                if (args.pathFormat !== 'path') {
                    this.sendErrorResponse(response, 2018, 'debug adapter only supports native paths', null, ErrorDestination.Telemetry);
                }
                else {
                    const initializeResponse = response;
                    initializeResponse.body = {};
                    this.initializeRequest(initializeResponse, args);
                }
            }
            else if (request.command === 'launch') {
                this.launchRequest(response, request.arguments, request);
            }
            else if (request.command === 'attach') {
                this.attachRequest(response, request.arguments, request);
            }
            else if (request.command === 'disconnect') {
                this.disconnectRequest(response, request.arguments, request);
            }
            else if (request.command === 'terminate') {
                this.terminateRequest(response, request.arguments, request);
            }
            else if (request.command === 'restart') {
                this.restartRequest(response, request.arguments, request);
            }
            else if (request.command === 'setBreakpoints') {
                this.setBreakPointsRequest(response, request.arguments, request);
            }
            else if (request.command === 'setFunctionBreakpoints') {
                this.setFunctionBreakPointsRequest(response, request.arguments, request);
            }
            else if (request.command === 'setExceptionBreakpoints') {
                this.setExceptionBreakPointsRequest(response, request.arguments, request);
            }
            else if (request.command === 'configurationDone') {
                this.configurationDoneRequest(response, request.arguments, request);
            }
            else if (request.command === 'continue') {
                this.continueRequest(response, request.arguments, request);
            }
            else if (request.command === 'next') {
                this.nextRequest(response, request.arguments, request);
            }
            else if (request.command === 'stepIn') {
                this.stepInRequest(response, request.arguments, request);
            }
            else if (request.command === 'stepOut') {
                this.stepOutRequest(response, request.arguments, request);
            }
            else if (request.command === 'stepBack') {
                this.stepBackRequest(response, request.arguments, request);
            }
            else if (request.command === 'reverseContinue') {
                this.reverseContinueRequest(response, request.arguments, request);
            }
            else if (request.command === 'restartFrame') {
                this.restartFrameRequest(response, request.arguments, request);
            }
            else if (request.command === 'goto') {
                this.gotoRequest(response, request.arguments, request);
            }
            else if (request.command === 'pause') {
                this.pauseRequest(response, request.arguments, request);
            }
            else if (request.command === 'stackTrace') {
                this.stackTraceRequest(response, request.arguments, request);
            }
            else if (request.command === 'scopes') {
                this.scopesRequest(response, request.arguments, request);
            }
            else if (request.command === 'variables') {
                this.variablesRequest(response, request.arguments, request);
            }
            else if (request.command === 'setVariable') {
                this.setVariableRequest(response, request.arguments, request);
            }
            else if (request.command === 'setExpression') {
                this.setExpressionRequest(response, request.arguments, request);
            }
            else if (request.command === 'source') {
                this.sourceRequest(response, request.arguments, request);
            }
            else if (request.command === 'threads') {
                this.threadsRequest(response, request);
            }
            else if (request.command === 'terminateThreads') {
                this.terminateThreadsRequest(response, request.arguments, request);
            }
            else if (request.command === 'evaluate') {
                this.evaluateRequest(response, request.arguments, request);
            }
            else if (request.command === 'stepInTargets') {
                this.stepInTargetsRequest(response, request.arguments, request);
            }
            else if (request.command === 'gotoTargets') {
                this.gotoTargetsRequest(response, request.arguments, request);
            }
            else if (request.command === 'completions') {
                this.completionsRequest(response, request.arguments, request);
            }
            else if (request.command === 'exceptionInfo') {
                this.exceptionInfoRequest(response, request.arguments, request);
            }
            else if (request.command === 'loadedSources') {
                this.loadedSourcesRequest(response, request.arguments, request);
            }
            else if (request.command === 'dataBreakpointInfo') {
                this.dataBreakpointInfoRequest(response, request.arguments, request);
            }
            else if (request.command === 'setDataBreakpoints') {
                this.setDataBreakpointsRequest(response, request.arguments, request);
            }
            else if (request.command === 'readMemory') {
                this.readMemoryRequest(response, request.arguments, request);
            }
            else if (request.command === 'disassemble') {
                this.disassembleRequest(response, request.arguments, request);
            }
            else if (request.command === 'cancel') {
                this.cancelRequest(response, request.arguments, request);
            }
            else if (request.command === 'breakpointLocations') {
                this.breakpointLocationsRequest(response, request.arguments, request);
            }
            else if (request.command === 'setInstructionBreakpoints') {
                this.setInstructionBreakpointsRequest(response, request.arguments, request);
            }
            else {
                this.customRequest(request.command, response, request.arguments, request);
            }
        }
        catch (e) {
            this.sendErrorResponse(response, 1104, '{_stack}', { _exception: e.message, _stack: e.stack }, ErrorDestination.Telemetry);
        }
    }
    initializeRequest(response, args) {
        // This default debug adapter does not support conditional breakpoints.
        response.body.supportsConditionalBreakpoints = false;
        // This default debug adapter does not support hit conditional breakpoints.
        response.body.supportsHitConditionalBreakpoints = false;
        // This default debug adapter does not support function breakpoints.
        response.body.supportsFunctionBreakpoints = false;
        // This default debug adapter implements the 'configurationDone' request.
        response.body.supportsConfigurationDoneRequest = true;
        // This default debug adapter does not support hovers based on the 'evaluate' request.
        response.body.supportsEvaluateForHovers = false;
        // This default debug adapter does not support the 'stepBack' request.
        response.body.supportsStepBack = false;
        // This default debug adapter does not support the 'setVariable' request.
        response.body.supportsSetVariable = false;
        // This default debug adapter does not support the 'restartFrame' request.
        response.body.supportsRestartFrame = false;
        // This default debug adapter does not support the 'stepInTargets' request.
        response.body.supportsStepInTargetsRequest = false;
        // This default debug adapter does not support the 'gotoTargets' request.
        response.body.supportsGotoTargetsRequest = false;
        // This default debug adapter does not support the 'completions' request.
        response.body.supportsCompletionsRequest = false;
        // This default debug adapter does not support the 'restart' request.
        response.body.supportsRestartRequest = false;
        // This default debug adapter does not support the 'exceptionOptions' attribute on the 'setExceptionBreakpoints' request.
        response.body.supportsExceptionOptions = false;
        // This default debug adapter does not support the 'format' attribute on the 'variables', 'evaluate', and 'stackTrace' request.
        response.body.supportsValueFormattingOptions = false;
        // This debug adapter does not support the 'exceptionInfo' request.
        response.body.supportsExceptionInfoRequest = false;
        // This debug adapter does not support the 'TerminateDebuggee' attribute on the 'disconnect' request.
        response.body.supportTerminateDebuggee = false;
        // This debug adapter does not support delayed loading of stack frames.
        response.body.supportsDelayedStackTraceLoading = false;
        // This debug adapter does not support the 'loadedSources' request.
        response.body.supportsLoadedSourcesRequest = false;
        // This debug adapter does not support the 'logMessage' attribute of the SourceBreakpoint.
        response.body.supportsLogPoints = false;
        // This debug adapter does not support the 'terminateThreads' request.
        response.body.supportsTerminateThreadsRequest = false;
        // This debug adapter does not support the 'setExpression' request.
        response.body.supportsSetExpression = false;
        // This debug adapter does not support the 'terminate' request.
        response.body.supportsTerminateRequest = false;
        // This debug adapter does not support data breakpoints.
        response.body.supportsDataBreakpoints = false;
        /** This debug adapter does not support the 'readMemory' request. */
        response.body.supportsReadMemoryRequest = false;
        /** The debug adapter does not support the 'disassemble' request. */
        response.body.supportsDisassembleRequest = false;
        /** The debug adapter does not support the 'cancel' request. */
        response.body.supportsCancelRequest = false;
        /** The debug adapter does not support the 'breakpointLocations' request. */
        response.body.supportsBreakpointLocationsRequest = false;
        /** The debug adapter does not support the 'clipboard' context value in the 'evaluate' request. */
        response.body.supportsClipboardContext = false;
        /** The debug adapter does not support stepping granularities for the stepping requests. */
        response.body.supportsSteppingGranularity = false;
        /** The debug adapter does not support the 'setInstructionBreakpoints' request. */
        response.body.supportsInstructionBreakpoints = false;
        this.sendResponse(response);
    }
    disconnectRequest(response, args, request) {
        this.sendResponse(response);
        this.shutdown();
    }
    launchRequest(response, args, request) {
        this.sendResponse(response);
    }
    attachRequest(response, args, request) {
        this.sendResponse(response);
    }
    terminateRequest(response, args, request) {
        this.sendResponse(response);
    }
    restartRequest(response, args, request) {
        this.sendResponse(response);
    }
    setBreakPointsRequest(response, args, request) {
        this.sendResponse(response);
    }
    setFunctionBreakPointsRequest(response, args, request) {
        this.sendResponse(response);
    }
    setExceptionBreakPointsRequest(response, args, request) {
        this.sendResponse(response);
    }
    configurationDoneRequest(response, args, request) {
        this.sendResponse(response);
    }
    continueRequest(response, args, request) {
        this.sendResponse(response);
    }
    nextRequest(response, args, request) {
        this.sendResponse(response);
    }
    stepInRequest(response, args, request) {
        this.sendResponse(response);
    }
    stepOutRequest(response, args, request) {
        this.sendResponse(response);
    }
    stepBackRequest(response, args, request) {
        this.sendResponse(response);
    }
    reverseContinueRequest(response, args, request) {
        this.sendResponse(response);
    }
    restartFrameRequest(response, args, request) {
        this.sendResponse(response);
    }
    gotoRequest(response, args, request) {
        this.sendResponse(response);
    }
    pauseRequest(response, args, request) {
        this.sendResponse(response);
    }
    sourceRequest(response, args, request) {
        this.sendResponse(response);
    }
    threadsRequest(response, request) {
        this.sendResponse(response);
    }
    terminateThreadsRequest(response, args, request) {
        this.sendResponse(response);
    }
    stackTraceRequest(response, args, request) {
        this.sendResponse(response);
    }
    scopesRequest(response, args, request) {
        this.sendResponse(response);
    }
    variablesRequest(response, args, request) {
        this.sendResponse(response);
    }
    setVariableRequest(response, args, request) {
        this.sendResponse(response);
    }
    setExpressionRequest(response, args, request) {
        this.sendResponse(response);
    }
    evaluateRequest(response, args, request) {
        this.sendResponse(response);
    }
    stepInTargetsRequest(response, args, request) {
        this.sendResponse(response);
    }
    gotoTargetsRequest(response, args, request) {
        this.sendResponse(response);
    }
    completionsRequest(response, args, request) {
        this.sendResponse(response);
    }
    exceptionInfoRequest(response, args, request) {
        this.sendResponse(response);
    }
    loadedSourcesRequest(response, args, request) {
        this.sendResponse(response);
    }
    dataBreakpointInfoRequest(response, args, request) {
        this.sendResponse(response);
    }
    setDataBreakpointsRequest(response, args, request) {
        this.sendResponse(response);
    }
    readMemoryRequest(response, args, request) {
        this.sendResponse(response);
    }
    disassembleRequest(response, args, request) {
        this.sendResponse(response);
    }
    cancelRequest(response, args, request) {
        this.sendResponse(response);
    }
    breakpointLocationsRequest(response, args, request) {
        this.sendResponse(response);
    }
    setInstructionBreakpointsRequest(response, args, request) {
        this.sendResponse(response);
    }
    /**
     * Override this hook to implement custom requests.
     */
    customRequest(command, response, args, request) {
        this.sendErrorResponse(response, 1014, 'unrecognized request', null, ErrorDestination.Telemetry);
    }
    //---- protected -------------------------------------------------------------------------------------------------
    convertClientLineToDebugger(line) {
        if (this._debuggerLinesStartAt1) {
            return this._clientLinesStartAt1 ? line : line + 1;
        }
        return this._clientLinesStartAt1 ? line - 1 : line;
    }
    convertDebuggerLineToClient(line) {
        if (this._debuggerLinesStartAt1) {
            return this._clientLinesStartAt1 ? line : line - 1;
        }
        return this._clientLinesStartAt1 ? line + 1 : line;
    }
    convertClientColumnToDebugger(column) {
        if (this._debuggerColumnsStartAt1) {
            return this._clientColumnsStartAt1 ? column : column + 1;
        }
        return this._clientColumnsStartAt1 ? column - 1 : column;
    }
    convertDebuggerColumnToClient(column) {
        if (this._debuggerColumnsStartAt1) {
            return this._clientColumnsStartAt1 ? column : column - 1;
        }
        return this._clientColumnsStartAt1 ? column + 1 : column;
    }
    convertClientPathToDebugger(clientPath) {
        if (this._clientPathsAreURIs !== this._debuggerPathsAreURIs) {
            if (this._clientPathsAreURIs) {
                return DebugSession.uri2path(clientPath);
            }
            else {
                return DebugSession.path2uri(clientPath);
            }
        }
        return clientPath;
    }
    convertDebuggerPathToClient(debuggerPath) {
        if (this._debuggerPathsAreURIs !== this._clientPathsAreURIs) {
            if (this._debuggerPathsAreURIs) {
                return DebugSession.uri2path(debuggerPath);
            }
            else {
                return DebugSession.path2uri(debuggerPath);
            }
        }
        return debuggerPath;
    }
    //---- private -------------------------------------------------------------------------------
    static path2uri(path) {
        if (process.platform === 'win32') {
            if (/^[A-Z]:/.test(path)) {
                path = path[0].toLowerCase() + path.substr(1);
            }
            path = path.replace(/\\/g, '/');
        }
        path = encodeURI(path);
        let uri = new url_1.URL(`file:`); // ignore 'path' for now
        uri.pathname = path; // now use 'path' to get the correct percent encoding (see https://url.spec.whatwg.org)
        return uri.toString();
    }
    static uri2path(sourceUri) {
        let uri = new url_1.URL(sourceUri);
        let s = decodeURIComponent(uri.pathname);
        if (process.platform === 'win32') {
            if (/^\/[a-zA-Z]:/.test(s)) {
                s = s[1].toLowerCase() + s.substr(2);
            }
            s = s.replace(/\//g, '\\');
        }
        return s;
    }
    /*
    * If argument starts with '_' it is OK to send its value to telemetry.
    */
    static formatPII(format, excludePII, args) {
        return format.replace(DebugSession._formatPIIRegexp, function (match, paramName) {
            if (excludePII && paramName.length > 0 && paramName[0] !== '_') {
                return match;
            }
            return args[paramName] && args.hasOwnProperty(paramName) ?
                args[paramName] :
                match;
        });
    }
}
exports.DebugSession = DebugSession;
DebugSession._formatPIIRegexp = /{([^}]+)}/g;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGVidWdTZXNzaW9uLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vc3JjL2RlYnVnU2Vzc2lvbi50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUE7OztnR0FHZ0c7OztBQUdoRyx5Q0FBNEM7QUFDNUMseUNBQTZDO0FBQzdDLHVEQUFvRDtBQUNwRCw2QkFBMEI7QUFHMUIsTUFBYSxNQUFNO0lBS2xCLFlBQW1CLElBQVksRUFBRSxJQUFhLEVBQUUsS0FBYSxDQUFDLEVBQUUsTUFBZSxFQUFFLElBQVU7UUFDMUYsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUM7UUFDakIsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUM7UUFDakIsSUFBSSxDQUFDLGVBQWUsR0FBRyxFQUFFLENBQUM7UUFDMUIsSUFBSSxNQUFNLEVBQUU7WUFDTCxJQUFLLENBQUMsTUFBTSxHQUFHLE1BQU0sQ0FBQztTQUM1QjtRQUNELElBQUksSUFBSSxFQUFFO1lBQ0gsSUFBSyxDQUFDLFdBQVcsR0FBRyxJQUFJLENBQUM7U0FDL0I7SUFDRixDQUFDO0NBQ0Q7QUFoQkQsd0JBZ0JDO0FBRUQsTUFBYSxLQUFLO0lBS2pCLFlBQW1CLElBQVksRUFBRSxTQUFpQixFQUFFLFlBQXFCLEtBQUs7UUFDN0UsSUFBSSxDQUFDLElBQUksR0FBRyxJQUFJLENBQUM7UUFDakIsSUFBSSxDQUFDLGtCQUFrQixHQUFHLFNBQVMsQ0FBQztRQUNwQyxJQUFJLENBQUMsU0FBUyxHQUFHLFNBQVMsQ0FBQztJQUM1QixDQUFDO0NBQ0Q7QUFWRCxzQkFVQztBQUVELE1BQWEsVUFBVTtJQU90QixZQUFtQixDQUFTLEVBQUUsRUFBVSxFQUFFLEdBQVksRUFBRSxLQUFhLENBQUMsRUFBRSxNQUFjLENBQUM7UUFDdEYsSUFBSSxDQUFDLEVBQUUsR0FBRyxDQUFDLENBQUM7UUFDWixJQUFJLENBQUMsTUFBTSxHQUFHLEdBQUcsQ0FBQztRQUNsQixJQUFJLENBQUMsSUFBSSxHQUFHLEVBQUUs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Q0FBQztRQUM3QixJQUFJLENBQUMsR0FBRyxrQkFBa0IsQ0FBQyxHQUFHLENBQUMsUUFBUSxDQUFDLENBQUM7UUFDekMsSUFBSSxPQUFPLENBQUMsUUFBUSxLQUFLLE9BQU8sRUFBRTtZQUNqQyxJQUFJLGNBQWMsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEVBQUU7Z0JBQzNCLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsV0FBVyxFQUFFLEdBQUcsQ0FBQyxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQzthQUNyQztZQUNELENBQUMsR0FBRyxDQUFDLENBQUMsT0FBTyxDQUFDLEtBQUssRUFBRSxJQUFJLENBQUMsQ0FBQztTQUMzQjtRQUNELE9BQU8sQ0FBQyxDQUFDO0lBQ1YsQ0FBQztJQUlEOztNQUVFO0lBQ00sTUFBTSxDQUFDLFNBQVMsQ0FBQyxNQUFhLEVBQUUsVUFBbUIsRUFBRSxJQUE2QjtRQUN6RixPQUFPLE1BQU0sQ0FBQyxPQUFPLENBQUMsWUFBWSxDQUFDLGdCQUFnQixFQUFFLFVBQVMsS0FBSyxFQUFFLFNBQVM7WUFDN0UsSUFBSSxVQUFVLElBQUksU0FBUyxDQUFDLE1BQU0sR0FBRyxDQUFDLElBQUksU0FBUyxDQUFDLENBQUMsQ0FBQyxLQUFLLEdBQUcsRUFBRTtnQkFDL0QsT0FBTyxLQUFLLENBQUM7YUFDYjtZQUNELE9BQU8sSUFBSSxDQUFDLFNBQVMsQ0FBQyxJQUFJLElBQUksQ0FBQyxjQUFjLENBQUMsU0FBUyxDQUFDLENBQUMsQ0FBQztnQkFDekQsSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUM7Z0JBQ2pCLEtBQUssQ0FBQztRQUNSLENBQUMsQ0FBQyxDQUFBO0lBQ0gsQ0FBQzs7QUE5bEJGLG9DQStsQkM7QUFmZSw2QkFBZ0IsR0FBRyxZQUFZLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyIvKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuICogIENvcHlyaWdodCAoYykgTWljcm9zb2Z0IENvcnBvcmF0aW9uLiBBbGwgcmlnaHRzIHJlc2VydmVkLlxuICogIExpY2Vuc2VkIHVuZGVyIHRoZSBNSVQgTGljZW5zZS4gU2VlIExpY2Vuc2UudHh0IGluIHRoZSBwcm9qZWN0IHJvb3QgZm9yIGxpY2Vuc2UgaW5mb3JtYXRpb24uXG4gKi0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tKi9cblxuaW1wb3J0IHsgRGVidWdQcm90b2NvbCB9IGZyb20gJ3ZzY29kZS1kZWJ1Z3Byb3RvY29sJztcbmltcG9ydCB7IFByb3RvY29sU2VydmVyIH0gZnJvbSAnLi9wcm90b2NvbCc7XG5pbXBvcnQgeyBSZXNwb25zZSwgRXZlbnQgfSBmcm9tICcuL21lc3NhZ2VzJztcbmltcG9ydCB7IHJ1bkRlYnVnQWRhcHRlciB9IGZyb20gJy4vcnVuRGVidWdBZGFwdGVyJztcbmltcG9ydCB7IFVSTCB9IGZyb20gJ3VybCc7XG5cblxuZXhwb3J0IGNsYXNzIFNvdXJjZSBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuU291cmNlIHtcblx0bmFtZTogc3RyaW5nO1xuXHRwYXRoOiBzdHJpbmc7XG5cdHNvdXJjZVJlZmVyZW5jZTogbnVtYmVyO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihuYW1lOiBzdHJpbmcsIHBhdGg/OiBzdHJpbmcsIGlkOiBudW1iZXIgPSAwLCBvcmlnaW4/OiBzdHJpbmcsIGRhdGE/OiBhbnkpIHtcblx0XHR0aGlzLm5hbWUgPSBuYW1lO1xuXHRcdHRoaXMucGF0aCA9IHBhdGg7XG5cdFx0dGhpcy5zb3VyY2VSZWZlcmVuY2UgPSBpZDtcblx0XHRpZiAob3JpZ2luKSB7XG5cdFx0XHQoPGFueT50aGlzKS5vcmlnaW4gPSBvcmlnaW47XG5cdFx0fVxuXHRcdGlmIChkYXRhKSB7XG5cdFx0XHQoPGFueT50aGlzKS5hZGFwdGVyRGF0YSA9IGRhdGE7XG5cdFx0fVxuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBTY29wZSBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuU2NvcGUge1xuXHRuYW1lOiBzdHJpbmc7XG5cdHZhcmlhYmxlc1JlZmVyZW5jZTogbnVtYmVyO1xuXHRleHBlbnNpdmU6IGJvb2xlYW47XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKG5hbWU6IHN0cmluZywgcmVmZXJlbmNlOiBudW1iZXIsIGV4cGVuc2l2ZTogYm9vbGVhbiA9IGZhbHNlKSB7XG5cdFx0dGhpcy5uYW1lID0gbmFtZTtcblx0XHR0aGlzLnZhcmlhYmxlc1JlZmVyZW5jZSA9IHJlZmVyZW5jZTtcblx0XHR0aGlzLmV4cGVuc2l2ZSA9IGV4cGVuc2l2ZTtcblx0fVxufVxuXG5leHBvcnQgY2xhc3MgU3RhY2tGcmFtZSBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuU3RhY2tGcmFtZSB7XG5cdGlkOiBudW1iZXI7XG5cdHNvdXJjZTogU291cmNlO1xuXHRsaW5lOiBudW1iZXI7XG5cdGNvbHVtbjogbnVtYmVyO1xuXHRuYW1lOiBzdHJpbmc7XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKGk6IG51bWJlciwgbm06IHN0cmluZywgc3JjPzogU291cmNlLCBsbjogbnVtYmVyID0gMCwgY29sOiBudW1iZXIgPSAwKSB7XG5cdFx0dGhpcy5pZCA9IGk7XG5cdFx0dGhpcy5zb3VyY2UgPSBzcmM7XG5cdFx0dGhpcy5saW5lID0gbG47XG5cdFx0dGhpcy5jb2x1bW4gPSBjb2w7XG5cdFx0dGhpcy5uYW1lID0gbm07XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIFRocmVhZCBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuVGhyZWFkIHtcblx0aWQ6IG51bWJlcjtcblx0bmFtZTogc3RyaW5nO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihpZDogbnVtYmVyLCBuYW1lOiBzdHJpbmcpIHtcblx0XHR0aGlzLmlkID0gaWQ7XG5cdFx0aWYgKG5hbWUpIHtcblx0XHRcdHRoaXMubmFtZSA9IG5hbWU7XG5cdFx0fSBlbHNlIHtcblx0XHRcdHRoaXMubmFtZSA9ICdUaHJlYWQgIycgKyBpZDtcblx0XHR9XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIFZhcmlhYmxlIGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5WYXJpYWJsZSB7XG5cdG5hbWU6IHN0cmluZztcblx0dmFsdWU6IHN0cmluZztcblx0dmFyaWFibGVzUmVmZXJlbmNlOiBudW1iZXI7XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKG5hbWU6IHN0cmluZywgdmFsdWU6IHN0cmluZywgcmVmOiBudW1iZXIgPSAwLCBpbmRleGVkVmFyaWFibGVzPzogbnVtYmVyLCBuYW1lZFZhcmlhYmxlcz86IG51bWJlcikge1xuXHRcdHRoaXMubmFtZSA9IG5hbWU7XG5cdFx0dGhpcy52YWx1ZSA9IHZhbHVlO1xuXHRcdHRoaXMudmFyaWFibGVzUmVmZXJlbmNlID0gcmVmO1xuXHRcdGlmICh0eXBlb2YgbmFtZWRWYXJpYWJsZXMgPT09ICdudW1iZXInKSB7XG5cdFx0XHQoPERlYnVnUHJvdG9jb2wuVmFyaWFibGU+dGhpcykubmFtZWRWYXJpYWJsZXMgPSBuYW1lZFZhcmlhYmxlcztcblx0XHR9XG5cdFx0aWYgKHR5cGVvZiBpbmRleGVkVmFyaWFibGVzID09PSAnbnVtYmVyJykge1xuXHRcdFx0KDxEZWJ1Z1Byb3RvY29sLlZhcmlhYmxlPnRoaXMpLmluZGV4ZWRWYXJpYWJsZXMgPSBpbmRleGVkVmFyaWFibGVzO1xuXHRcdH1cblx0fVxufVxuXG5leHBvcnQgY2xhc3MgQnJlYWtwb2ludCBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuQnJlYWtwb2ludCB7XG5cdHZlcmlmaWVkOiBib29sZWFuO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3Rvcih2ZXJpZmllZDogYm9vbGVhbiwgbGluZT86IG51bWJlciwgY29sdW1uPzogbnVtYmVyLCBzb3VyY2U/OiBTb3VyY2UpIHtcblx0XHR0aGlzLnZlcmlmaWVkID0gdmVyaWZpZWQ7XG5cdFx0Y29uc3QgZTogRGVidWdQcm90b2NvbC5CcmVha3BvaW50ID0gdGhpcztcblx0XHRpZiAodHlwZW9mIGxpbmUgPT09ICdudW1iZXInKSB7XG5cdFx0XHRlLmxpbmUgPSBsaW5lO1xuXHRcdH1cblx0XHRpZiAodHlwZW9mIGNvbHVtbiA9PT0gJ251bWJlcicpIHtcblx0XHRcdGUuY29sdW1uID0gY29sdW1uO1xuXHRcdH1cblx0XHRpZiAoc291cmNlKSB7XG5cdFx0XHRlLnNvdXJjZSA9IHNvdXJjZTtcblx0XHR9XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIE1vZHVsZSBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuTW9kdWxlIHtcblx0aWQ6IG51bWJlciB8IHN0cmluZztcblx0bmFtZTogc3RyaW5nO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihpZDogbnVtYmVyIHwgc3RyaW5nLCBuYW1lOiBzdHJpbmcpIHtcblx0XHR0aGlzLmlkID0gaWQ7XG5cdFx0dGhpcy5uYW1lID0gbmFtZTtcblx0fVxufVxuXG5leHBvcnQgY2xhc3MgQ29tcGxldGlvbkl0ZW0gaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLkNvbXBsZXRpb25JdGVtIHtcblx0bGFiZWw6IHN0cmluZztcblx0c3RhcnQ6IG51bWJlcjtcblx0bGVuZ3RoOiBudW1iZXI7XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKGxhYmVsOiBzdHJpbmcsIHN0YXJ0OiBudW1iZXIsIGxlbmd0aDogbnVtYmVyID0gMCkge1xuXHRcdHRoaXMubGFiZWwgPSBsYWJlbDtcblx0XHR0aGlzLnN0YXJ0ID0gc3RhcnQ7XG5cdFx0dGhpcy5sZW5ndGggPSBsZW5ndGg7XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIFN0b3BwZWRFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5TdG9wcGVkRXZlbnQge1xuXHRib2R5OiB7XG5cdFx0cmVhc29uOiBzdHJpbmc7XG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHJlYXNvbjogc3RyaW5nLCB0aHJlYWRJZD86IG51bWJlciwgZXhjZXB0aW9uVGV4dD86IHN0cmluZykge1xuXHRcdHN1cGVyKCdzdG9wcGVkJyk7XG5cdFx0dGhpcy5ib2R5ID0ge1xuXHRcdFx0cmVhc29uOiByZWFzb25cblx0XHR9O1xuXHRcdGlmICh0eXBlb2YgdGhyZWFkSWQgPT09ICdudW1iZXInKSB7XG5cdFx0XHQodGhpcyBhcyBEZWJ1Z1Byb3RvY29sLlN0b3BwZWRFdmVudCkuYm9keS50aHJlYWRJZCA9IHRocmVhZElkO1xuXHRcdH1cblx0XHRpZiAodHlwZW9mIGV4Y2VwdGlvblRleHQgPT09ICdzdHJpbmcnKSB7XG5cdFx0XHQodGhpcyBhcyBEZWJ1Z1Byb3RvY29sLlN0b3BwZWRFdmVudCkuYm9keS50ZXh0ID0gZXhjZXB0aW9uVGV4dDtcblx0XHR9XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIENvbnRpbnVlZEV2ZW50IGV4dGVuZHMgRXZlbnQgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLkNvbnRpbnVlZEV2ZW50IHtcblx0Ym9keToge1xuXHRcdHRocmVhZElkOiBudW1iZXI7XG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHRocmVhZElkOiBudW1iZXIsIGFsbFRocmVhZHNDb250aW51ZWQ/OiBib29sZWFuKSB7XG5cdFx0c3VwZXIoJ2NvbnRpbnVlZCcpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdHRocmVhZElkOiB0aHJlYWRJZFxuXHRcdH07XG5cblx0XHRpZiAodHlwZW9mIGFsbFRocmVhZHNDb250aW51ZWQgPT09ICdib29sZWFuJykge1xuXHRcdFx0KDxEZWJ1Z1Byb3RvY29sLkNvbnRpbnVlZEV2ZW50PnRoaXMpLmJvZHkuYWxsVGhyZWFkc0NvbnRpbnVlZCA9IGFsbFRocmVhZHNDb250aW51ZWQ7XG5cdFx0fVxuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBJbml0aWFsaXplZEV2ZW50IGV4dGVuZHMgRXZlbnQgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLkluaXRpYWxpemVkRXZlbnQge1xuXHRwdWJsaWMgY29uc3RydWN0b3IoKSB7XG5cdFx0c3VwZXIoJ2luaXRpYWxpemVkJyk7XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIFRlcm1pbmF0ZWRFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5UZXJtaW5hdGVkRXZlbnQge1xuXHRwdWJsaWMgY29uc3RydWN0b3IocmVzdGFydD86IGFueSkge1xuXHRcdHN1cGVyKCd0ZXJtaW5hdGVkJyk7XG5cdFx0aWYgKHR5cGVvZiByZXN0YXJ0ID09PSAnYm9vbGVhbicgfHwgcmVzdGFydCkge1xuXHRcdFx0Y29uc3QgZTogRGVidWdQcm90b2NvbC5UZXJtaW5hdGVkRXZlbnQgPSB0aGlzO1xuXHRcdFx0ZS5ib2R5ID0ge1xuXHRcdFx0XHRyZXN0YXJ0OiByZXN0YXJ0XG5cdFx0XHR9O1xuXHRcdH1cblx0fVxufVxuXG5leHBvcnQgY2xhc3MgT3V0cHV0RXZlbnQgZXh0ZW5kcyBFdmVudCBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuT3V0cHV0RXZlbnQge1xuXHRib2R5OiB7XG5cdFx0Y2F0ZWdvcnk6IHN0cmluZyxcblx0XHRvdXRwdXQ6IHN0cmluZyxcblx0XHRkYXRhPzogYW55XG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKG91dHB1dDogc3RyaW5nLCBjYXRlZ29yeTogc3RyaW5nID0gJ2NvbnNvbGUnLCBkYXRhPzogYW55KSB7XG5cdFx0c3VwZXIoJ291dHB1dCcpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdGNhdGVnb3J5OiBjYXRlZ29yeSxcblx0XHRcdG91dHB1dDogb3V0cHV0XG5cdFx0fTtcblx0XHRpZiAoZGF0YSAhPT0gdW5kZWZpbmVkKSB7XG5cdFx0XHR0aGlzLmJvZHkuZGF0YSA9IGRhdGE7XG5cdFx0fVxuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBUaHJlYWRFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5UaHJlYWRFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRyZWFzb246IHN0cmluZyxcblx0XHR0aHJlYWRJZDogbnVtYmVyXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHJlYXNvbjogc3RyaW5nLCB0aHJlYWRJZDogbnVtYmVyKSB7XG5cdFx0c3VwZXIoJ3RocmVhZCcpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdHJlYXNvbjogcmVhc29uLFxuXHRcdFx0dGhyZWFkSWQ6IHRocmVhZElkXG5cdFx0fTtcblx0fVxufVxuXG5leHBvcnQgY2xhc3MgQnJlYWtwb2ludEV2ZW50IGV4dGVuZHMgRXZlbnQgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLkJyZWFrcG9pbnRFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRyZWFzb246IHN0cmluZyxcblx0XHRicmVha3BvaW50OiBCcmVha3BvaW50XG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHJlYXNvbjogc3RyaW5nLCBicmVha3BvaW50OiBCcmVha3BvaW50KSB7XG5cdFx0c3VwZXIoJ2JyZWFrcG9pbnQnKTtcblx0XHR0aGlzLmJvZHkgPSB7XG5cdFx0XHRyZWFzb246IHJlYXNvbixcblx0XHRcdGJyZWFrcG9pbnQ6IGJyZWFrcG9pbnRcblx0XHR9O1xuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBNb2R1bGVFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5Nb2R1bGVFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRyZWFzb246ICduZXcnIHwgJ2NoYW5nZWQnIHwgJ3JlbW92ZWQnLFxuXHRcdG1vZHVsZTogTW9kdWxlXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHJlYXNvbjogJ25ldycgfCAnY2hhbmdlZCcgfCAncmVtb3ZlZCcsIG1vZHVsZTogTW9kdWxlKSB7XG5cdFx0c3VwZXIoJ21vZHVsZScpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdHJlYXNvbjogcmVhc29uLFxuXHRcdFx0bW9kdWxlOiBtb2R1bGVcblx0XHR9O1xuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBMb2FkZWRTb3VyY2VFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5Mb2FkZWRTb3VyY2VFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRyZWFzb246ICduZXcnIHwgJ2NoYW5nZWQnIHwgJ3JlbW92ZWQnLFxuXHRcdHNvdXJjZTogU291cmNlXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHJlYXNvbjogJ25ldycgfCAnY2hhbmdlZCcgfCAncmVtb3ZlZCcsIHNvdXJjZTogU291cmNlKSB7XG5cdFx0c3VwZXIoJ2xvYWRlZFNvdXJjZScpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdHJlYXNvbjogcmVhc29uLFxuXHRcdFx0c291cmNlOiBzb3VyY2Vcblx0XHR9O1xuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBDYXBhYmlsaXRpZXNFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5DYXBhYmlsaXRpZXNFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRjYXBhYmlsaXRpZXM6IERlYnVnUHJvdG9jb2wuQ2FwYWJpbGl0aWVzXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKGNhcGFiaWxpdGllczogRGVidWdQcm90b2NvbC5DYXBhYmlsaXRpZXMpIHtcblx0XHRzdXBlcignY2FwYWJpbGl0aWVzJyk7XG5cdFx0dGhpcy5ib2R5ID0ge1xuXHRcdFx0Y2FwYWJpbGl0aWVzOiBjYXBhYmlsaXRpZXNcblx0XHR9O1xuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBQcm9ncmVzc1N0YXJ0RXZlbnQgZXh0ZW5kcyBFdmVudCBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuUHJvZ3Jlc3NTdGFydEV2ZW50IHtcblx0Ym9keToge1xuXHRcdHByb2dyZXNzSWQ6IHN0cmluZyxcblx0XHR0aXRsZTogc3RyaW5nXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHByb2dyZXNzSWQ6IHN0cmluZywgdGl0bGU6IHN0cmluZywgbWVzc2FnZT86IHN0cmluZykge1xuXHRcdHN1cGVyKCdwcm9ncmVzc1N0YXJ0Jyk7XG5cdFx0dGhpcy5ib2R5ID0ge1xuXHRcdFx0cHJvZ3Jlc3NJZDogcHJvZ3Jlc3NJZCxcblx0XHRcdHRpdGxlOiB0aXRsZVxuXHRcdH07XG5cdFx0aWYgKHR5cGVvZiBtZXNzYWdlID09PSAnc3RyaW5nJykge1xuXHRcdFx0KHRoaXMgYXMgRGVidWdQcm90b2NvbC5Qcm9ncmVzc1N0YXJ0RXZlbnQpLmJvZHkubWVzc2FnZSA9IG1lc3NhZ2U7XG5cdFx0fVxuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBQcm9ncmVzc1VwZGF0ZUV2ZW50IGV4dGVuZHMgRXZlbnQgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLlByb2dyZXNzVXBkYXRlRXZlbnQge1xuXHRib2R5OiB7XG5cdFx0cHJvZ3Jlc3NJZDogc3RyaW5nXG5cdH07XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHByb2dyZXNzSWQ6IHN0cmluZywgbWVzc2FnZT86IHN0cmluZykge1xuXHRcdHN1cGVyKCdwcm9ncmVzc1VwZGF0ZScpO1xuXHRcdHRoaXMuYm9keSA9IHtcblx0XHRcdHByb2dyZXNzSWQ6IHByb2dyZXNzSWRcblx0XHR9O1xuXHRcdGlmICh0eXBlb2YgbWVzc2FnZSA9PT0gJ3N0cmluZycpIHtcblx0XHRcdCh0aGlzIGFzIERlYnVnUHJvdG9jb2wuUHJvZ3Jlc3NVcGRhdGVFdmVudCkuYm9keS5tZXNzYWdlID0gbWVzc2FnZTtcblx0XHR9XG5cdH1cbn1cblxuZXhwb3J0IGNsYXNzIFByb2dyZXNzRW5kRXZlbnQgZXh0ZW5kcyBFdmVudCBpbXBsZW1lbnRzIERlYnVnUHJvdG9jb2wuUHJvZ3Jlc3NFbmRFdmVudCB7XG5cdGJvZHk6IHtcblx0XHRwcm9ncmVzc0lkOiBzdHJpbmdcblx0fTtcblxuXHRwdWJsaWMgY29uc3RydWN0b3IocHJvZ3Jlc3NJZDogc3RyaW5nLCBtZXNzYWdlPzogc3RyaW5nKSB7XG5cdFx0c3VwZXIoJ3Byb2dyZXNzRW5kJyk7XG5cdFx0dGhpcy5ib2R5ID0ge1xuXHRcdFx0cHJvZ3Jlc3NJZDogcHJvZ3Jlc3NJZFxuXHRcdH07XG5cdFx0aWYgKHR5cGVvZiBtZXNzYWdlID09PSAnc3RyaW5nJykge1xuXHRcdFx0KHRoaXMgYXMgRGVidWdQcm90b2NvbC5Qcm9ncmVzc0VuZEV2ZW50KS5ib2R5Lm1lc3NhZ2UgPSBtZXNzYWdlO1xuXHRcdH1cblx0fVxufVxuXG5leHBvcnQgY2xhc3MgSW52YWxpZGF0ZWRFdmVudCBleHRlbmRzIEV2ZW50IGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5JbnZhbGlkYXRlZEV2ZW50IHtcblx0Ym9keToge1xuXHRcdGFyZWFzPzogRGVidWdQcm90b2NvbC5JbnZhbGlkYXRlZEFyZWFzW107XG5cdFx0dGhyZWFkSWQ/OiBudW1iZXI7XG5cdFx0c3RhY2tGcmFtZUlkPzogbnVtYmVyO1xuXHR9O1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihhcmVhcz86IERlYnVnUHJvdG9jb2wuSW52YWxpZGF0ZWRBcmVhc1tdLCB0aHJlYWRJZD86IG51bWJlciwgc3RhY2tGcmFtZUlkPzogbnVtYmVyKSB7XG5cdFx0c3VwZXIoJ2ludmFsaWRhdGVkJyk7XG5cdFx0dGhpcy5ib2R5ID0ge1xuXHRcdH07XG5cdFx0aWYgKGFyZWFzKSB7XG5cdFx0XHR0aGlzLmJvZHkuYXJlYXMgPSBhcmVhcztcblx0XHR9XG5cdFx0aWYgKHRocmVhZElkKSB7XG5cdFx0XHR0aGlzLmJvZHkudGhyZWFkSWQgPSB0aHJlYWRJZDtcblx0XHR9XG5cdFx0aWYgKHN0YWNrRnJhbWVJZCkge1xuXHRcdFx0dGhpcy5ib2R5LnN0YWNrRnJhbWVJZCA9IHN0YWNrRnJhbWVJZDtcblx0XHR9XG5cdH1cbn1cblxuZXhwb3J0IGVudW0gRXJyb3JEZXN0aW5hdGlvbiB7XG5cdFVzZXIgPSAxLFxuXHRUZWxlbWV0cnkgPSAyXG59O1xuXG5leHBvcnQgY2xhc3MgRGVidWdTZXNzaW9uIGV4dGVuZHMgUHJvdG9jb2xTZXJ2ZXIge1xuXG5cdHByaXZhdGUgX2RlYnVnZ2VyTGluZXNTdGFydEF0MTogYm9vbGVhbjtcblx0cHJpdmF0ZSBfZGVidWdnZXJDb2x1bW5zU3RhcnRBdDE6IGJvb2xlYW47XG5cdHByaXZhdGUgX2RlYnVnZ2VyUGF0aHNBcmVVUklzOiBib29sZWFuO1xuXG5cdHByaXZhdGUgX2NsaWVudExpbmVzU3RhcnRBdDE6IGJvb2xlYW47XG5cdHByaXZhdGUgX2NsaWVudENvbHVtbnNTdGFydEF0MTogYm9vbGVhbjtcblx0cHJpdmF0ZSBfY2xpZW50UGF0aHNBcmVVUklzOiBib29sZWFuO1xuXG5cdHByb3RlY3RlZCBfaXNTZXJ2ZXI6IGJvb2xlYW47XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKG9ic29sZXRlX2RlYnVnZ2VyTGluZXNBbmRDb2x1bW5zU3RhcnRBdDE/OiBib29sZWFuLCBvYnNvbGV0ZV9pc1NlcnZlcj86IGJvb2xlYW4pIHtcblx0XHRzdXBlcigpO1xuXG5cdFx0Y29uc3QgbGluZXNBbmRDb2x1bW5zU3RhcnRBdDEgPSB0eXBlb2Ygb2Jzb2xldGVfZGVidWdnZXJMaW5lc0FuZENvbHVtbnNTdGFydEF0MSA9PT0gJ2Jvb2xlYW4nID8gb2Jzb2xldGVfZGVidWdnZXJMaW5lc0FuZENvbHVtbnNTdGFydEF0MSA6IGZhbHNlO1xuXHRcdHRoaXMuX2RlYnVnZ2VyTGluZXNTdGFydEF0MSA9IGxpbmVzQW5kQ29sdW1uc1N0YXJ0QXQxO1xuXHRcdHRoaXMuX2RlYnVnZ2VyQ29sdW1uc1N0YXJ0QXQxID0gbGluZXNBbmRDb2x1bW5zU3RhcnRBdDE7XG5cdFx0dGhpcy5fZGVidWdnZXJQYXRoc0FyZVVSSXMgPSBmYWxzZTtcblxuXHRcdHRoaXMuX2NsaWVudExpbmVzU3RhcnRBdDEgPSB0cnVlO1xuXHRcdHRoaXMuX2NsaWVudENvbHVtbnNTdGFydEF0MSA9IHRydWU7XG5cdFx0dGhpcy5fY2xpZW50UGF0aHNBcmVVUklzID0gZmFsc2U7XG5cblx0XHR0aGlzLl9pc1NlcnZlciA9IHR5cGVvZiBvYnNvbGV0ZV9pc1NlcnZlciA9PT0gJ2Jvb2xlYW4nID8gb2Jzb2xldGVfaXNTZXJ2ZXIgOiBmYWxzZTtcblxuXHRcdHRoaXMub24oJ2Nsb3NlJywgKCkgPT4ge1xuXHRcdFx0dGhpcy5zaHV0ZG93bigpO1xuXHRcdH0pO1xuXHRcdHRoaXMub24oJ2Vycm9yJywgKGVycm9yKSA9PiB7XG5cdFx0XHR0aGlzLnNodXRkb3duKCk7XG5cdFx0fSk7XG5cdH1cblxuXHRwdWJsaWMgc2V0RGVidWdnZXJQYXRoRm9ybWF0KGZvcm1hdDogc3RyaW5nKSB7XG5cdFx0dGhpcy5fZGVidWdnZXJQYXRoc0FyZVVSSXMgPSBmb3JtYXQgIT09ICdwYXRoJztcblx0fVxuXG5cdHB1YmxpYyBzZXREZWJ1Z2dlckxpbmVzU3RhcnRBdDEoZW5hYmxlOiBib29sZWFuKSB7XG5cdFx0dGhpcy5fZGVidWdnZXJMaW5lc1N0YXJ0QXQxID0gZW5hYmxlO1xuXHR9XG5cblx0cHVibGljIHNldERlYnVnZ2VyQ29sdW1uc1N0YXJ0QXQxKGVuYWJsZTogYm9vbGVhbikge1xuXHRcdHRoaXMuX2RlYnVnZ2VyQ29sdW1uc1N0YXJ0QXQxID0gZW5hYmxlO1xuXHR9XG5cblx0cHVibGljIHNldFJ1bkFzU2VydmVyKGVuYWJsZTogYm9vbGVhbikge1xuXHRcdHRoaXMuX2lzU2VydmVyID0gZW5hYmxlO1xuXHR9XG5cblx0LyoqXG5cdCAqIEEgdmlydHVhbCBjb25zdHJ1Y3Rvci4uLlxuXHQgKi9cblx0cHVibGljIHN0YXRpYyBydW4oZGVidWdTZXNzaW9uOiB0eXBlb2YgRGVidWdTZXNzaW9uKSB7XG5cdFx0cnVuRGVidWdBZGFwdGVyKGRlYnVnU2Vzc2lvbik7XG5cdH1cblxuXHRwdWJsaWMgc2h1dGRvd24oKTogdm9pZCB7XG5cdFx0aWYgKHRoaXMuX2lzU2VydmVyIHx8IHRoaXMuX2lzUnVubmluZ0lubGluZSgpKSB7XG5cdFx0XHQvLyBzaHV0ZG93biBpZ25vcmVkIGluIHNlcnZlciBtb2RlXG5cdFx0fSBlbHNlIHtcblx0XHRcdC8vIHdhaXQgYSBiaXQgYmVmb3JlIHNodXR0aW5nIGRvd25cblx0XHRcdHNldFRpbWVvdXQoKCkgPT4ge1xuXHRcdFx0XHRwcm9jZXNzLmV4aXQoMCk7XG5cdFx0XHR9LCAxMDApO1xuXHRcdH1cblx0fVxuXG5cdHByb3RlY3RlZCBzZW5kRXJyb3JSZXNwb25zZShyZXNwb25zZTogRGVidWdQcm90b2NvbC5SZXNwb25zZSwgY29kZU9yTWVzc2FnZTogbnVtYmVyIHwgRGVidWdQcm90b2NvbC5NZXNzYWdlLCBmb3JtYXQ/OiBzdHJpbmcsIHZhcmlhYmxlcz86IGFueSwgZGVzdDogRXJyb3JEZXN0aW5hdGlvbiA9IEVycm9yRGVzdGluYXRpb24uVXNlcik6IHZvaWQge1xuXG5cdFx0bGV0IG1zZyA6IERlYnVnUHJvdG9jb2wuTWVzc2FnZTtcblx0XHRpZiAodHlwZW9mIGNvZGVPck1lc3NhZ2UgPT09ICdudW1iZXInKSB7XG5cdFx0XHRtc2cgPSA8RGVidWdQcm90b2NvbC5NZXNzYWdlPiB7XG5cdFx0XHRcdGlkOiA8bnVtYmVyPiBjb2RlT3JNZXNzYWdlLFxuXHRcdFx0XHRmb3JtYXQ6IGZvcm1hdFxuXHRcdFx0fTtcblx0XHRcdGlmICh2YXJpYWJsZXMpIHtcblx0XHRcdFx0bXNnLnZhcmlhYmxlcyA9IHZhcmlhYmxlcztcblx0XHRcdH1cblx0XHRcdGlmIChkZXN0ICYgRXJyb3JEZXN0aW5hdGlvbi5Vc2VyKSB7XG5cdFx0XHRcdG1zZy5zaG93VXNlciA9IHRydWU7XG5cdFx0XHR9XG5cdFx0XHRpZiAoZGVzdCAmIEVycm9yRGVzdGluYXRpb24uVGVsZW1ldHJ5KSB7XG5cdFx0XHRcdG1zZy5zZW5kVGVsZW1ldHJ5ID0gdHJ1ZTtcblx0XHRcdH1cblx0XHR9IGVsc2Uge1xuXHRcdFx0bXNnID0gY29kZU9yTWVzc2FnZTtcblx0XHR9XG5cblx0XHRyZXNwb25zZS5zdWNjZXNzID0gZmFsc2U7XG5cdFx0cmVzcG9uc2UubWVzc2FnZSA9IERlYnVnU2Vzc2lvbi5mb3JtYXRQSUkobXNnLmZvcm1hdCwgdHJ1ZSwgbXNnLnZhcmlhYmxlcyk7XG5cdFx0aWYgKCFyZXNwb25zZS5ib2R5KSB7XG5cdFx0XHRyZXNwb25zZS5ib2R5ID0geyB9O1xuXHRcdH1cblx0XHRyZXNwb25zZS5ib2R5LmVycm9yID0gbXNnO1xuXG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHVibGljIHJ1bkluVGVybWluYWxSZXF1ZXN0KGFyZ3M6IERlYnVnUHJvdG9jb2wuUnVuSW5UZXJtaW5hbFJlcXVlc3RBcmd1bWVudHMsIHRpbWVvdXQ6IG51bWJlciwgY2I6IChyZXNwb25zZTogRGVidWdQcm90b2NvbC5SdW5JblRlcm1pbmFsUmVzcG9uc2UpID0+IHZvaWQpIHtcblx0XHR0aGlzLnNlbmRSZXF1ZXN0KCdydW5JblRlcm1pbmFsJywgYXJncywgdGltZW91dCwgY2IpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGRpc3BhdGNoUmVxdWVzdChyZXF1ZXN0OiBEZWJ1Z1Byb3RvY29sLlJlcXVlc3QpOiB2b2lkIHtcblxuXHRcdGNvbnN0IHJlc3BvbnNlID0gbmV3IFJlc3BvbnNlKHJlcXVlc3QpO1xuXG5cdFx0dHJ5IHtcblx0XHRcdGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdpbml0aWFsaXplJykge1xuXHRcdFx0XHR2YXIgYXJncyA9IDxEZWJ1Z1Byb3RvY29sLkluaXRpYWxpemVSZXF1ZXN0QXJndW1lbnRzPiByZXF1ZXN0LmFyZ3VtZW50cztcblxuXHRcdFx0XHRpZiAodHlwZW9mIGFyZ3MubGluZXNTdGFydEF0MSA9PT0gJ2Jvb2xlYW4nKSB7XG5cdFx0XHRcdFx0dGhpcy5fY2xpZW50TGluZXNTdGFydEF0MSA9IGFyZ3MubGluZXNTdGFydEF0MTtcblx0XHRcdFx0fVxuXHRcdFx0XHRpZiAodHlwZW9mIGFyZ3MuY29sdW1uc1N0YXJ0QXQxID09PSAnYm9vbGVhbicpIHtcblx0XHRcdFx0XHR0aGlzLl9jbGllbnRDb2x1bW5zU3RhcnRBdDEgPSBhcmdzLmNvbHVtbnNTdGFydEF0MTtcblx0XHRcdFx0fVxuXG5cdFx0XHRcdGlmIChhcmdzLnBhdGhGb3JtYXQgIT09ICdwYXRoJykge1xuXHRcdFx0XHRcdHRoaXMuc2VuZEVycm9yUmVzcG9uc2UocmVzcG9uc2UsIDIwMTgsICdkZWJ1ZyBhZGFwdGVyIG9ubHkgc3VwcG9ydHMgbmF0aXZlIHBhdGhzJywgbnVsbCwgRXJyb3JEZXN0aW5hdGlvbi5UZWxlbWV0cnkpO1xuXHRcdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRcdGNvbnN0IGluaXRpYWxpemVSZXNwb25zZSA9IDxEZWJ1Z1Byb3RvY29sLkluaXRpYWxpemVSZXNwb25zZT4gcmVzcG9uc2U7XG5cdFx0XHRcdFx0aW5pdGlhbGl6ZVJlc3BvbnNlLmJvZHkgPSB7fTtcblx0XHRcdFx0XHR0aGlzLmluaXRpYWxpemVSZXF1ZXN0KGluaXRpYWxpemVSZXNwb25zZSwgYXJncyk7XG5cdFx0XHRcdH1cblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdsYXVuY2gnKSB7XG5cdFx0XHRcdHRoaXMubGF1bmNoUmVxdWVzdCg8RGVidWdQcm90b2NvbC5MYXVuY2hSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdhdHRhY2gnKSB7XG5cdFx0XHRcdHRoaXMuYXR0YWNoUmVxdWVzdCg8RGVidWdQcm90b2NvbC5BdHRhY2hSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdkaXNjb25uZWN0Jykge1xuXHRcdFx0XHR0aGlzLmRpc2Nvbm5lY3RSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLkRpc2Nvbm5lY3RSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICd0ZXJtaW5hdGUnKSB7XG5cdFx0XHRcdHRoaXMudGVybWluYXRlUmVxdWVzdCg8RGVidWdQcm90b2NvbC5UZXJtaW5hdGVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdyZXN0YXJ0Jykge1xuXHRcdFx0XHR0aGlzLnJlc3RhcnRSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlJlc3RhcnRSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzZXRCcmVha3BvaW50cycpIHtcblx0XHRcdFx0dGhpcy5zZXRCcmVha1BvaW50c1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuU2V0QnJlYWtwb2ludHNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzZXRGdW5jdGlvbkJyZWFrcG9pbnRzJykge1xuXHRcdFx0XHR0aGlzLnNldEZ1bmN0aW9uQnJlYWtQb2ludHNSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlNldEZ1bmN0aW9uQnJlYWtwb2ludHNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzZXRFeGNlcHRpb25CcmVha3BvaW50cycpIHtcblx0XHRcdFx0dGhpcy5zZXRFeGNlcHRpb25CcmVha1BvaW50c1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuU2V0RXhjZXB0aW9uQnJlYWtwb2ludHNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdjb25maWd1cmF0aW9uRG9uZScpIHtcblx0XHRcdFx0dGhpcy5jb25maWd1cmF0aW9uRG9uZVJlcXVlc3QoPERlYnVnUHJvdG9jb2wuQ29uZmlndXJhdGlvbkRvbmVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdjb250aW51ZScpIHtcblx0XHRcdFx0dGhpcy5jb250aW51ZVJlcXVlc3QoPERlYnVnUHJvdG9jb2wuQ29udGludWVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICduZXh0Jykge1xuXHRcdFx0XHR0aGlzLm5leHRSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLk5leHRSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzdGVwSW4nKSB7XG5cdFx0XHRcdHRoaXMuc3RlcEluUmVxdWVzdCg8RGVidWdQcm90b2NvbC5TdGVwSW5SZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzdGVwT3V0Jykge1xuXHRcdFx0XHR0aGlzLnN0ZXBPdXRSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlN0ZXBPdXRSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzdGVwQmFjaycpIHtcblx0XHRcdFx0dGhpcy5zdGVwQmFja1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuU3RlcEJhY2tSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdyZXZlcnNlQ29udGludWUnKSB7XG5cdFx0XHRcdHRoaXMucmV2ZXJzZUNvbnRpbnVlUmVxdWVzdCg8RGVidWdQcm90b2NvbC5SZXZlcnNlQ29udGludWVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdyZXN0YXJ0RnJhbWUnKSB7XG5cdFx0XHRcdHRoaXMucmVzdGFydEZyYW1lUmVxdWVzdCg8RGVidWdQcm90b2NvbC5SZXN0YXJ0RnJhbWVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdnb3RvJykge1xuXHRcdFx0XHR0aGlzLmdvdG9SZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLkdvdG9SZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdwYXVzZScpIHtcblx0XHRcdFx0dGhpcy5wYXVzZVJlcXVlc3QoPERlYnVnUHJvdG9jb2wuUGF1c2VSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzdGFja1RyYWNlJykge1xuXHRcdFx0XHR0aGlzLnN0YWNrVHJhY2VSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlN0YWNrVHJhY2VSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzY29wZXMnKSB7XG5cdFx0XHRcdHRoaXMuc2NvcGVzUmVxdWVzdCg8RGVidWdQcm90b2NvbC5TY29wZXNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICd2YXJpYWJsZXMnKSB7XG5cdFx0XHRcdHRoaXMudmFyaWFibGVzUmVxdWVzdCg8RGVidWdQcm90b2NvbC5WYXJpYWJsZXNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzZXRWYXJpYWJsZScpIHtcblx0XHRcdFx0dGhpcy5zZXRWYXJpYWJsZVJlcXVlc3QoPERlYnVnUHJvdG9jb2wuU2V0VmFyaWFibGVSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzZXRFeHByZXNzaW9uJykge1xuXHRcdFx0XHR0aGlzLnNldEV4cHJlc3Npb25SZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlNldEV4cHJlc3Npb25SZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICdzb3VyY2UnKSB7XG5cdFx0XHRcdHRoaXMuc291cmNlUmVxdWVzdCg8RGVidWdQcm90b2NvbC5Tb3VyY2VSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblxuXHRcdFx0fSBlbHNlIGlmIChyZXF1ZXN0LmNvbW1hbmQgPT09ICd0aHJlYWRzJykge1xuXHRcdFx0XHR0aGlzLnRocmVhZHNSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlRocmVhZHNSZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ3Rlcm1pbmF0ZVRocmVhZHMnKSB7XG5cdFx0XHRcdHRoaXMudGVybWluYXRlVGhyZWFkc1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuVGVybWluYXRlVGhyZWFkc1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2V2YWx1YXRlJykge1xuXHRcdFx0XHR0aGlzLmV2YWx1YXRlUmVxdWVzdCg8RGVidWdQcm90b2NvbC5FdmFsdWF0ZVJlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ3N0ZXBJblRhcmdldHMnKSB7XG5cdFx0XHRcdHRoaXMuc3RlcEluVGFyZ2V0c1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuU3RlcEluVGFyZ2V0c1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2dvdG9UYXJnZXRzJykge1xuXHRcdFx0XHR0aGlzLmdvdG9UYXJnZXRzUmVxdWVzdCg8RGVidWdQcm90b2NvbC5Hb3RvVGFyZ2V0c1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2NvbXBsZXRpb25zJykge1xuXHRcdFx0XHR0aGlzLmNvbXBsZXRpb25zUmVxdWVzdCg8RGVidWdQcm90b2NvbC5Db21wbGV0aW9uc1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2V4Y2VwdGlvbkluZm8nKSB7XG5cdFx0XHRcdHRoaXMuZXhjZXB0aW9uSW5mb1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuRXhjZXB0aW9uSW5mb1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2xvYWRlZFNvdXJjZXMnKSB7XG5cdFx0XHRcdHRoaXMubG9hZGVkU291cmNlc1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuTG9hZGVkU291cmNlc1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2RhdGFCcmVha3BvaW50SW5mbycpIHtcblx0XHRcdFx0dGhpcy5kYXRhQnJlYWtwb2ludEluZm9SZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLkRhdGFCcmVha3BvaW50SW5mb1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ3NldERhdGFCcmVha3BvaW50cycpIHtcblx0XHRcdFx0dGhpcy5zZXREYXRhQnJlYWtwb2ludHNSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLlNldERhdGFCcmVha3BvaW50c1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ3JlYWRNZW1vcnknKSB7XG5cdFx0XHRcdHRoaXMucmVhZE1lbW9yeVJlcXVlc3QoPERlYnVnUHJvdG9jb2wuUmVhZE1lbW9yeVJlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2Rpc2Fzc2VtYmxlJykge1xuXHRcdFx0XHR0aGlzLmRpc2Fzc2VtYmxlUmVxdWVzdCg8RGVidWdQcm90b2NvbC5EaXNhc3NlbWJsZVJlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2NhbmNlbCcpIHtcblx0XHRcdFx0dGhpcy5jYW5jZWxSZXF1ZXN0KDxEZWJ1Z1Byb3RvY29sLkNhbmNlbFJlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ2JyZWFrcG9pbnRMb2NhdGlvbnMnKSB7XG5cdFx0XHRcdHRoaXMuYnJlYWtwb2ludExvY2F0aW9uc1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuQnJlYWtwb2ludExvY2F0aW9uc1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2UgaWYgKHJlcXVlc3QuY29tbWFuZCA9PT0gJ3NldEluc3RydWN0aW9uQnJlYWtwb2ludHMnKSB7XG5cdFx0XHRcdHRoaXMuc2V0SW5zdHJ1Y3Rpb25CcmVha3BvaW50c1JlcXVlc3QoPERlYnVnUHJvdG9jb2wuU2V0SW5zdHJ1Y3Rpb25CcmVha3BvaW50c1Jlc3BvbnNlPiByZXNwb25zZSwgcmVxdWVzdC5hcmd1bWVudHMsIHJlcXVlc3QpO1xuXG5cdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHR0aGlzLmN1c3RvbVJlcXVlc3QocmVxdWVzdC5jb21tYW5kLCA8RGVidWdQcm90b2NvbC5SZXNwb25zZT4gcmVzcG9uc2UsIHJlcXVlc3QuYXJndW1lbnRzLCByZXF1ZXN0KTtcblx0XHRcdH1cblx0XHR9IGNhdGNoIChlKSB7XG5cdFx0XHR0aGlzLnNlbmRFcnJvclJlc3BvbnNlKHJlc3BvbnNlLCAxMTA0LCAne19zdGFja30nLCB7IF9leGNlcHRpb246IGUubWVzc2FnZSwgX3N0YWNrOiBlLnN0YWNrIH0sIEVycm9yRGVzdGluYXRpb24uVGVsZW1ldHJ5KTtcblx0XHR9XG5cdH1cblxuXHRwcm90ZWN0ZWQgaW5pdGlhbGl6ZVJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuSW5pdGlhbGl6ZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkluaXRpYWxpemVSZXF1ZXN0QXJndW1lbnRzKTogdm9pZCB7XG5cblx0XHQvLyBUaGlzIGRlZmF1bHQgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IGNvbmRpdGlvbmFsIGJyZWFrcG9pbnRzLlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNDb25kaXRpb25hbEJyZWFrcG9pbnRzID0gZmFsc2U7XG5cblx0XHQvLyBUaGlzIGRlZmF1bHQgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IGhpdCBjb25kaXRpb25hbCBicmVha3BvaW50cy5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzSGl0Q29uZGl0aW9uYWxCcmVha3BvaW50cyA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCBmdW5jdGlvbiBicmVha3BvaW50cy5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzRnVuY3Rpb25CcmVha3BvaW50cyA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgaW1wbGVtZW50cyB0aGUgJ2NvbmZpZ3VyYXRpb25Eb25lJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNDb25maWd1cmF0aW9uRG9uZVJlcXVlc3QgPSB0cnVlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCBob3ZlcnMgYmFzZWQgb24gdGhlICdldmFsdWF0ZScgcmVxdWVzdC5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzRXZhbHVhdGVGb3JIb3ZlcnMgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVmYXVsdCBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdzdGVwQmFjaycgcmVxdWVzdC5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzU3RlcEJhY2sgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVmYXVsdCBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdzZXRWYXJpYWJsZScgcmVxdWVzdC5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzU2V0VmFyaWFibGUgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVmYXVsdCBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdyZXN0YXJ0RnJhbWUnIHJlcXVlc3QuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c1Jlc3RhcnRGcmFtZSA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ3N0ZXBJblRhcmdldHMnIHJlcXVlc3QuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c1N0ZXBJblRhcmdldHNSZXF1ZXN0ID0gZmFsc2U7XG5cblx0XHQvLyBUaGlzIGRlZmF1bHQgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IHRoZSAnZ290b1RhcmdldHMnIHJlcXVlc3QuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c0dvdG9UYXJnZXRzUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ2NvbXBsZXRpb25zJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNDb21wbGV0aW9uc1JlcXVlc3QgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVmYXVsdCBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdyZXN0YXJ0JyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNSZXN0YXJ0UmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWZhdWx0IGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ2V4Y2VwdGlvbk9wdGlvbnMnIGF0dHJpYnV0ZSBvbiB0aGUgJ3NldEV4Y2VwdGlvbkJyZWFrcG9pbnRzJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNFeGNlcHRpb25PcHRpb25zID0gZmFsc2U7XG5cblx0XHQvLyBUaGlzIGRlZmF1bHQgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IHRoZSAnZm9ybWF0JyBhdHRyaWJ1dGUgb24gdGhlICd2YXJpYWJsZXMnLCAnZXZhbHVhdGUnLCBhbmQgJ3N0YWNrVHJhY2UnIHJlcXVlc3QuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c1ZhbHVlRm9ybWF0dGluZ09wdGlvbnMgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IHRoZSAnZXhjZXB0aW9uSW5mbycgcmVxdWVzdC5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzRXhjZXB0aW9uSW5mb1JlcXVlc3QgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IHRoZSAnVGVybWluYXRlRGVidWdnZWUnIGF0dHJpYnV0ZSBvbiB0aGUgJ2Rpc2Nvbm5lY3QnIHJlcXVlc3QuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0VGVybWluYXRlRGVidWdnZWUgPSBmYWxzZTtcblxuXHRcdC8vIFRoaXMgZGVidWcgYWRhcHRlciBkb2VzIG5vdCBzdXBwb3J0IGRlbGF5ZWQgbG9hZGluZyBvZiBzdGFjayBmcmFtZXMuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c0RlbGF5ZWRTdGFja1RyYWNlTG9hZGluZyA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdsb2FkZWRTb3VyY2VzJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNMb2FkZWRTb3VyY2VzUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdsb2dNZXNzYWdlJyBhdHRyaWJ1dGUgb2YgdGhlIFNvdXJjZUJyZWFrcG9pbnQuXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c0xvZ1BvaW50cyA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICd0ZXJtaW5hdGVUaHJlYWRzJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNUZXJtaW5hdGVUaHJlYWRzUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdzZXRFeHByZXNzaW9uJyByZXF1ZXN0LlxuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNTZXRFeHByZXNzaW9uID0gZmFsc2U7XG5cblx0XHQvLyBUaGlzIGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ3Rlcm1pbmF0ZScgcmVxdWVzdC5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzVGVybWluYXRlUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0Ly8gVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgZGF0YSBicmVha3BvaW50cy5cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzRGF0YUJyZWFrcG9pbnRzID0gZmFsc2U7XG5cblx0XHQvKiogVGhpcyBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdyZWFkTWVtb3J5JyByZXF1ZXN0LiAqL1xuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNSZWFkTWVtb3J5UmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0LyoqIFRoZSBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdkaXNhc3NlbWJsZScgcmVxdWVzdC4gKi9cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzRGlzYXNzZW1ibGVSZXF1ZXN0ID0gZmFsc2U7XG5cblx0XHQvKiogVGhlIGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ2NhbmNlbCcgcmVxdWVzdC4gKi9cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzQ2FuY2VsUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0LyoqIFRoZSBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdicmVha3BvaW50TG9jYXRpb25zJyByZXF1ZXN0LiAqL1xuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNCcmVha3BvaW50TG9jYXRpb25zUmVxdWVzdCA9IGZhbHNlO1xuXG5cdFx0LyoqIFRoZSBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgdGhlICdjbGlwYm9hcmQnIGNvbnRleHQgdmFsdWUgaW4gdGhlICdldmFsdWF0ZScgcmVxdWVzdC4gKi9cblx0XHRyZXNwb25zZS5ib2R5LnN1cHBvcnRzQ2xpcGJvYXJkQ29udGV4dCA9IGZhbHNlO1xuXG5cdFx0LyoqIFRoZSBkZWJ1ZyBhZGFwdGVyIGRvZXMgbm90IHN1cHBvcnQgc3RlcHBpbmcgZ3JhbnVsYXJpdGllcyBmb3IgdGhlIHN0ZXBwaW5nIHJlcXVlc3RzLiAqL1xuXHRcdHJlc3BvbnNlLmJvZHkuc3VwcG9ydHNTdGVwcGluZ0dyYW51bGFyaXR5ID0gZmFsc2U7XG5cblx0XHQvKiogVGhlIGRlYnVnIGFkYXB0ZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgJ3NldEluc3RydWN0aW9uQnJlYWtwb2ludHMnIHJlcXVlc3QuICovXG5cdFx0cmVzcG9uc2UuYm9keS5zdXBwb3J0c0luc3RydWN0aW9uQnJlYWtwb2ludHMgPSBmYWxzZTtcblxuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBkaXNjb25uZWN0UmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5EaXNjb25uZWN0UmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuRGlzY29ubmVjdEFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0XHR0aGlzLnNodXRkb3duKCk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgbGF1bmNoUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5MYXVuY2hSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5MYXVuY2hSZXF1ZXN0QXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGF0dGFjaFJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuQXR0YWNoUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuQXR0YWNoUmVxdWVzdEFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCB0ZXJtaW5hdGVSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlRlcm1pbmF0ZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLlRlcm1pbmF0ZUFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCByZXN0YXJ0UmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5SZXN0YXJ0UmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuUmVzdGFydEFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBzZXRCcmVha1BvaW50c1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuU2V0QnJlYWtwb2ludHNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TZXRCcmVha3BvaW50c0FyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBzZXRGdW5jdGlvbkJyZWFrUG9pbnRzUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5TZXRGdW5jdGlvbkJyZWFrcG9pbnRzUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU2V0RnVuY3Rpb25CcmVha3BvaW50c0FyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBzZXRFeGNlcHRpb25CcmVha1BvaW50c1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuU2V0RXhjZXB0aW9uQnJlYWtwb2ludHNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TZXRFeGNlcHRpb25CcmVha3BvaW50c0FyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBjb25maWd1cmF0aW9uRG9uZVJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuQ29uZmlndXJhdGlvbkRvbmVSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5Db25maWd1cmF0aW9uRG9uZUFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCk6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBjb250aW51ZVJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuQ29udGludWVSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5Db250aW51ZUFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCkgOiB2b2lkIHtcblx0XHR0aGlzLnNlbmRSZXNwb25zZShyZXNwb25zZSk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgbmV4dFJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuTmV4dFJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLk5leHRBcmd1bWVudHMsIHJlcXVlc3Q/OiBEZWJ1Z1Byb3RvY29sLlJlcXVlc3QpIDogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHN0ZXBJblJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuU3RlcEluUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU3RlcEluQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KSA6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBzdGVwT3V0UmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5TdGVwT3V0UmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU3RlcE91dEFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCkgOiB2b2lkIHtcblx0XHR0aGlzLnNlbmRSZXNwb25zZShyZXNwb25zZSk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgc3RlcEJhY2tSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlN0ZXBCYWNrUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU3RlcEJhY2tBcmd1bWVudHMsIHJlcXVlc3Q/OiBEZWJ1Z1Byb3RvY29sLlJlcXVlc3QpIDogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHJldmVyc2VDb250aW51ZVJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuUmV2ZXJzZUNvbnRpbnVlUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuUmV2ZXJzZUNvbnRpbnVlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KSA6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCByZXN0YXJ0RnJhbWVSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlJlc3RhcnRGcmFtZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLlJlc3RhcnRGcmFtZUFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCkgOiB2b2lkIHtcblx0XHR0aGlzLnNlbmRSZXNwb25zZShyZXNwb25zZSk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgZ290b1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuR290b1Jlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkdvdG9Bcmd1bWVudHMsIHJlcXVlc3Q/OiBEZWJ1Z1Byb3RvY29sLlJlcXVlc3QpIDogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHBhdXNlUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5QYXVzZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLlBhdXNlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KSA6IHZvaWQge1xuXHRcdHRoaXMuc2VuZFJlc3BvbnNlKHJlc3BvbnNlKTtcblx0fVxuXG5cdHByb3RlY3RlZCBzb3VyY2VSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlNvdXJjZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLlNvdXJjZUFyZ3VtZW50cywgcmVxdWVzdD86IERlYnVnUHJvdG9jb2wuUmVxdWVzdCkgOiB2b2lkIHtcblx0XHR0aGlzLnNlbmRSZXNwb25zZShyZXNwb25zZSk7XG5cdH1cblxuXHRwcm90ZWN0ZWQgdGhyZWFkc1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuVGhyZWFkc1Jlc3BvbnNlLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHRlcm1pbmF0ZVRocmVhZHNSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlRlcm1pbmF0ZVRocmVhZHNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5UZXJtaW5hdGVUaHJlYWRzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHN0YWNrVHJhY2VSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlN0YWNrVHJhY2VSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TdGFja1RyYWNlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHNjb3Blc1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuU2NvcGVzUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU2NvcGVzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHZhcmlhYmxlc1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuVmFyaWFibGVzUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuVmFyaWFibGVzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHNldFZhcmlhYmxlUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5TZXRWYXJpYWJsZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLlNldFZhcmlhYmxlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHNldEV4cHJlc3Npb25SZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlNldEV4cHJlc3Npb25SZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TZXRFeHByZXNzaW9uQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGV2YWx1YXRlUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5FdmFsdWF0ZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkV2YWx1YXRlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHN0ZXBJblRhcmdldHNSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlN0ZXBJblRhcmdldHNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TdGVwSW5UYXJnZXRzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGdvdG9UYXJnZXRzUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5Hb3RvVGFyZ2V0c1Jlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkdvdG9UYXJnZXRzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGNvbXBsZXRpb25zUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5Db21wbGV0aW9uc1Jlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkNvbXBsZXRpb25zQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGV4Y2VwdGlvbkluZm9SZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLkV4Y2VwdGlvbkluZm9SZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5FeGNlcHRpb25JbmZvQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGxvYWRlZFNvdXJjZXNSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLkxvYWRlZFNvdXJjZXNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5Mb2FkZWRTb3VyY2VzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGRhdGFCcmVha3BvaW50SW5mb1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuRGF0YUJyZWFrcG9pbnRJbmZvUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuRGF0YUJyZWFrcG9pbnRJbmZvQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHNldERhdGFCcmVha3BvaW50c1JlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuU2V0RGF0YUJyZWFrcG9pbnRzUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuU2V0RGF0YUJyZWFrcG9pbnRzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHJlYWRNZW1vcnlSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlJlYWRNZW1vcnlSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5SZWFkTWVtb3J5QXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGRpc2Fzc2VtYmxlUmVxdWVzdChyZXNwb25zZTogRGVidWdQcm90b2NvbC5EaXNhc3NlbWJsZVJlc3BvbnNlLCBhcmdzOiBEZWJ1Z1Byb3RvY29sLkRpc2Fzc2VtYmxlQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGNhbmNlbFJlcXVlc3QocmVzcG9uc2U6IERlYnVnUHJvdG9jb2wuQ2FuY2VsUmVzcG9uc2UsIGFyZ3M6IERlYnVnUHJvdG9jb2wuQ2FuY2VsQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIGJyZWFrcG9pbnRMb2NhdGlvbnNSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLkJyZWFrcG9pbnRMb2NhdGlvbnNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5CcmVha3BvaW50TG9jYXRpb25zQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0cHJvdGVjdGVkIHNldEluc3RydWN0aW9uQnJlYWtwb2ludHNSZXF1ZXN0KHJlc3BvbnNlOiBEZWJ1Z1Byb3RvY29sLlNldEluc3RydWN0aW9uQnJlYWtwb2ludHNSZXNwb25zZSwgYXJnczogRGVidWdQcm90b2NvbC5TZXRJbnN0cnVjdGlvbkJyZWFrcG9pbnRzQXJndW1lbnRzLCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kUmVzcG9uc2UocmVzcG9uc2UpO1xuXHR9XG5cblx0LyoqXG5cdCAqIE92ZXJyaWRlIHRoaXMgaG9vayB0byBpbXBsZW1lbnQgY3VzdG9tIHJlcXVlc3RzLlxuXHQgKi9cblx0cHJvdGVjdGVkIGN1c3RvbVJlcXVlc3QoY29tbWFuZDogc3RyaW5nLCByZXNwb25zZTogRGVidWdQcm90b2NvbC5SZXNwb25zZSwgYXJnczogYW55LCByZXF1ZXN0PzogRGVidWdQcm90b2NvbC5SZXF1ZXN0KTogdm9pZCB7XG5cdFx0dGhpcy5zZW5kRXJyb3JSZXNwb25zZShyZXNwb25zZSwgMTAxNCwgJ3VucmVjb2duaXplZCByZXF1ZXN0JywgbnVsbCwgRXJyb3JEZXN0aW5hdGlvbi5UZWxlbWV0cnkpO1xuXHR9XG5cblx0Ly8tLS0tIHByb3RlY3RlZCAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG5cblx0cHJvdGVjdGVkIGNvbnZlcnRDbGllbnRMaW5lVG9EZWJ1Z2dlcihsaW5lOiBudW1iZXIpOiBudW1iZXIge1xuXHRcdGlmICh0aGlzLl9kZWJ1Z2dlckxpbmVzU3RhcnRBdDEpIHtcblx0XHRcdHJldHVybiB0aGlzLl9jbGllbnRMaW5lc1N0YXJ0QXQxID8gbGluZSA6IGxpbmUgKyAxO1xuXHRcdH1cblx0XHRyZXR1cm4gdGhpcy5fY2xpZW50TGluZXNTdGFydEF0MSA/IGxpbmUgLSAxIDogbGluZTtcblx0fVxuXG5cdHByb3RlY3RlZCBjb252ZXJ0RGVidWdnZXJMaW5lVG9DbGllbnQobGluZTogbnVtYmVyKTogbnVtYmVyIHtcblx0XHRpZiAodGhpcy5fZGVidWdnZXJMaW5lc1N0YXJ0QXQxKSB7XG5cdFx0XHRyZXR1cm4gdGhpcy5fY2xpZW50TGluZXNTdGFydEF0MSA/IGxpbmUgOiBsaW5lIC0gMTtcblx0XHR9XG5cdFx0cmV0dXJuIHRoaXMuX2NsaWVudExpbmVzU3RhcnRBdDEgPyBsaW5lICsgMSA6IGxpbmU7XG5cdH1cblxuXHRwcm90ZWN0ZWQgY29udmVydENsaWVudENvbHVtblRvRGVidWdnZXIoY29sdW1uOiBudW1iZXIpOiBudW1iZXIge1xuXHRcdGlmICh0aGlzLl9kZWJ1Z2dlckNvbHVtbnNTdGFydEF0MSkge1xuXHRcdFx0cmV0dXJuIHRoaXMuX2NsaWVudENvbHVtbnNTdGFydEF0MSA/IGNvbHVtbiA6IGNvbHVtbiArIDE7XG5cdFx0fVxuXHRcdHJldHVybiB0aGlzLl9jbGllbnRDb2x1bW5zU3RhcnRBdDEgPyBjb2x1bW4gLSAxIDogY29sdW1uO1xuXHR9XG5cblx0cHJvdGVjdGVkIGNvbnZlcnREZWJ1Z2dlckNvbHVtblRvQ2xpZW50KGNvbHVtbjogbnVtYmVyKTogbnVtYmVyIHtcblx0XHRpZiAodGhpcy5fZGVidWdnZXJDb2x1bW5zU3RhcnRBdDEpIHtcblx0XHRcdHJldHVybiB0aGlzLl9jbGllbnRDb2x1bW5zU3RhcnRBdDEgPyBjb2x1bW4gOiBjb2x1bW4gLSAxO1xuXHRcdH1cblx0XHRyZXR1cm4gdGhpcy5fY2xpZW50Q29sdW1uc1N0YXJ0QXQxID8gY29sdW1uICsgMSA6IGNvbHVtbjtcblx0fVxuXG5cdHByb3RlY3RlZCBjb252ZXJ0Q2xpZW50UGF0aFRvRGVidWdnZXIoY2xpZW50UGF0aDogc3RyaW5nKTogc3RyaW5nIHtcblx0XHRpZiAodGhpcy5fY2xpZW50UGF0aHNBcmVVUklzICE9PSB0aGlzLl9kZWJ1Z2dlclBhdGhzQXJlVVJJcykge1xuXHRcdFx0aWYgKHRoaXMuX2NsaWVudFBhdGhzQXJlVVJJcykge1xuXHRcdFx0XHRyZXR1cm4gRGVidWdTZXNzaW9uLnVyaTJwYXRoKGNsaWVudFBhdGgpO1xuXHRcdFx0fSBlbHNlIHtcblx0XHRcdFx0cmV0dXJuIERlYnVnU2Vzc2lvbi5wYXRoMnVyaShjbGllbnRQYXRoKTtcblx0XHRcdH1cblx0XHR9XG5cdFx0cmV0dXJuIGNsaWVudFBhdGg7XG5cdH1cblxuXHRwcm90ZWN0ZWQgY29udmVydERlYnVnZ2VyUGF0aFRvQ2xpZW50KGRlYnVnZ2VyUGF0aDogc3RyaW5nKTogc3RyaW5nIHtcblx0XHRpZiAodGhpcy5fZGVidWdnZXJQYXRoc0FyZVVSSXMgIT09IHRoaXMuX2NsaWVudFBhdGhzQXJlVVJJcykge1xuXHRcdFx0aWYgKHRoaXMuX2RlYnVnZ2VyUGF0aHNBcmVVUklzKSB7XG5cdFx0XHRcdHJldHVybiBEZWJ1Z1Nlc3Npb24udXJpMnBhdGgoZGVidWdnZXJQYXRoKTtcblx0XHRcdH0gZWxzZSB7XG5cdFx0XHRcdHJldHVybiBEZWJ1Z1Nlc3Npb24ucGF0aDJ1cmkoZGVidWdnZXJQYXRoKTtcblx0XHRcdH1cblx0XHR9XG5cdFx0cmV0dXJuIGRlYnVnZ2VyUGF0aDtcblx0fVxuXG5cdC8vLS0tLSBwcml2YXRlIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cblxuXHRwcml2YXRlIHN0YXRpYyBwYXRoMnVyaShwYXRoOiBzdHJpbmcpOiBzdHJpbmcge1xuXG5cdFx0aWYgKHByb2Nlc3MucGxhdGZvcm0gPT09ICd3aW4zMicpIHtcblx0XHRcdGlmICgvXltBLVpdOi8udGVzdChwYXRoKSkge1xuXHRcdFx0XHRwYXRoID0gcGF0aFswXS50b0xvd2VyQ2FzZSgpICsgcGF0aC5zdWJzdHIoMSk7XG5cdFx0XHR9XG5cdFx0XHRwYXRoID0gcGF0aC5yZXBsYWNlKC9cXFxcL2csICcvJyk7XG5cdFx0fVxuXHRcdHBhdGggPSBlbmNvZGVVUkkocGF0aCk7XG5cblx0XHRsZXQgdXJpID0gbmV3IFVSTChgZmlsZTpgKTtcdC8vIGlnbm9yZSAncGF0aCcgZm9yIG5vd1xuXHRcdHVyaS5wYXRobmFtZSA9IHBhdGg7XHQvLyBub3cgdXNlICdwYXRoJyB0byBnZXQgdGhlIGNvcnJlY3QgcGVyY2VudCBlbmNvZGluZyAoc2VlIGh0dHBzOi8vdXJsLnNwZWMud2hhdHdnLm9yZylcblx0XHRyZXR1cm4gdXJpLnRvU3RyaW5nKCk7XG5cdH1cblxuXHRwcml2YXRlIHN0YXRpYyB1cmkycGF0aChzb3VyY2VVcmk6IHN0cmluZyk6IHN0cmluZyB7XG5cblx0XHRsZXQgdXJpID0gbmV3IFVSTChzb3VyY2VVcmkpO1xuXHRcdGxldCBzID0gZGVjb2RlVVJJQ29tcG9uZW50KHVyaS5wYXRobmFtZSk7XG5cdFx0aWYgKHByb2Nlc3MucGxhdGZvcm0gPT09ICd3aW4zMicpIHtcblx0XHRcdGlmICgvXlxcL1thLXpBLVpdOi8udGVzdChzKSkge1xuXHRcdFx0XHRzID0gc1sxXS50b0xvd2VyQ2FzZSgpICsgcy5zdWJzdHIoMik7XG5cdFx0XHR9XG5cdFx0XHRzID0gcy5yZXBsYWNlKC9cXC8vZywgJ1xcXFwnKTtcblx0XHR9XG5cdFx0cmV0dXJuIHM7XG5cdH1cblxuXHRwcml2YXRlIHN0YXRpYyBfZm9ybWF0UElJUmVnZXhwID0gL3soW159XSspfS9nO1xuXG5cdC8qXG5cdCogSWYgYXJndW1lbnQgc3RhcnRzIHdpdGggJ18nIGl0IGlzIE9LIHRvIHNlbmQgaXRzIHZhbHVlIHRvIHRlbGVtZXRyeS5cblx0Ki9cblx0cHJpdmF0ZSBzdGF0aWMgZm9ybWF0UElJKGZvcm1hdDpzdHJpbmcsIGV4Y2x1ZGVQSUk6IGJvb2xlYW4sIGFyZ3M6IHtba2V5OiBzdHJpbmddOiBzdHJpbmd9KTogc3RyaW5nIHtcblx0XHRyZXR1cm4gZm9ybWF0LnJlcGxhY2UoRGVidWdTZXNzaW9uLl9mb3JtYXRQSUlSZWdleHAsIGZ1bmN0aW9uKG1hdGNoLCBwYXJhbU5hbWUpIHtcblx0XHRcdGlmIChleGNsdWRlUElJICYmIHBhcmFtTmFtZS5sZW5ndGggPiAwICYmIHBhcmFtTmFtZVswXSAhPT0gJ18nKSB7XG5cdFx0XHRcdHJldHVybiBtYXRjaDtcblx0XHRcdH1cblx0XHRcdHJldHVybiBhcmdzW3BhcmFtTmFtZV0gJiYgYXJncy5oYXNPd25Qcm9wZXJ0eShwYXJhbU5hbWUpID9cblx0XHRcdFx0YXJnc1twYXJhbU5hbWVdIDpcblx0XHRcdFx0bWF0Y2g7XG5cdFx0fSlcblx0fVxufVxuIl19

/***/ }),

/***/ 47:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.Event = exports.Response = exports.Message = void 0;
class Message {
    constructor(type) {
        this.seq = 0;
        this.type = type;
    }
}
exports.Message = Message;
class Response extends Message {
    constructor(request, message) {
        super('response');
        this.request_seq = request.seq;
        this.command = request.command;
        if (message) {
            this.success = false;
            this.message = message;
        }
        else {
            this.success = true;
        }
    }
}
exports.Response = Response;
class Event extends Message {
    constructor(event, body) {
        super('event');
        this.event = event;
        if (body) {
            this.body = body;
        }
    }
}
exports.Event = Event;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWVzc2FnZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi9zcmMvbWVzc2FnZXMudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IjtBQUFBOzs7Z0dBR2dHOzs7QUFLaEcsTUFBYSxPQUFPO0lBSW5CLFlBQW1CLElBQVk7UUFDOUIsSUFBSSxDQUFDLEdBQUcsR0FBRyxDQUFDLENBQUM7UUFDYixJQUFJLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQztJQUNsQixDQUFDO0NBQ0Q7QUFSRCwwQkFRQztBQUVELE1BQWEsUUFBUyxTQUFRLE9BQU87SUFLcEMsWUFBbUIsT0FBOEIsRUFBRSxPQUFnQjtRQUNsRSxLQUFLLENBQUMsVUFBVSxDQUFDLENBQUM7UUFDbEIsSUFBSSxDQUFDLFdBQVcsR0FBRyxPQUFPLENBQUMsR0FBRyxDQUFDO1FBQy9CLElBQUksQ0FBQyxPQUFPLEdBQUcsT0FBTyxDQUFDLE9BQU8sQ0FBQztRQUMvQixJQUFJLE9BQU8sRUFBRTtZQUNaLElBQUksQ0FBQyxPQUFPLEdBQUcsS0FBSyxDQUFDO1lBQ2YsSUFBSyxDQUFDLE9BQU8sR0FBRyxPQUFPLENBQUM7U0FDOUI7YUFBTTtZQUNOLElBQUksQ0FBQyxPQUFPLEdBQUcsSUFBSSxDQUFDO1NBQ3BCO0lBQ0YsQ0FBQztDQUNEO0FBaEJELDRCQWdCQztBQUVELE1BQWEsS0FBTSxTQUFRLE9BQU87SUFHakMsWUFBbUIsS0FBYSxFQUFFLElBQVU7UUFDM0MsS0FBSyxDQUFDLE9BQU8sQ0FBQyxDQUFDO1FBQ2YsSUFBSSxDQUFDLEtBQUssR0FBRyxLQUFLLENBQUM7UUFDbkIsSUFBSSxJQUFJLEVBQUU7WUFDSCxJQUFLLENBQUMsSUFBSSxHQUFHLElBQUksQ0FBQztTQUN4QjtJQUNGLENBQUM7Q0FDRDtBQVZELHNCQVVDIiwic291cmNlc0NvbnRlbnQiOlsiLyotLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiAqICBDb3B5cmlnaHQgKGMpIE1pY3Jvc29mdCBDb3Jwb3JhdGlvbi4gQWxsIHJpZ2h0cyByZXNlcnZlZC5cbiAqICBMaWNlbnNlZCB1bmRlciB0aGUgTUlUIExpY2Vuc2UuIFNlZSBMaWNlbnNlLnR4dCBpbiB0aGUgcHJvamVjdCByb290IGZvciBsaWNlbnNlIGluZm9ybWF0aW9uLlxuICotLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLSovXG5cbmltcG9ydCB7IERlYnVnUHJvdG9jb2wgfSBmcm9tICd2c2NvZGUtZGVidWdwcm90b2NvbCc7XG5cblxuZXhwb3J0IGNsYXNzIE1lc3NhZ2UgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLlByb3RvY29sTWVzc2FnZSB7XG5cdHNlcTogbnVtYmVyO1xuXHR0eXBlOiBzdHJpbmc7XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKHR5cGU6IHN0cmluZykge1xuXHRcdHRoaXMuc2VxID0gMDtcblx0XHR0aGlzLnR5cGUgPSB0eXBlO1xuXHR9XG59XG5cbmV4cG9ydCBjbGFzcyBSZXNwb25zZSBleHRlbmRzIE1lc3NhZ2UgaW1wbGVtZW50cyBEZWJ1Z1Byb3RvY29sLlJlc3BvbnNlIHtcblx0cmVxdWVzdF9zZXE6IG51bWJlcjtcblx0c3VjY2VzczogYm9vbGVhbjtcblx0Y29tbWFuZDogc3RyaW5nO1xuXG5cdHB1YmxpYyBjb25zdHJ1Y3RvcihyZXF1ZXN0OiBEZWJ1Z1Byb3RvY29sLlJlcXVlc3QsIG1lc3NhZ2U/OiBzdHJpbmcpIHtcblx0XHRzdXBlcigncmVzcG9uc2UnKTtcblx0XHR0aGlzLnJlcXVlc3Rfc2VxID0gcmVxdWVzdC5zZXE7XG5cdFx0dGhpcy5jb21tYW5kID0gcmVxdWVzdC5jb21tYW5kO1xuXHRcdGlmIChtZXNzYWdlKSB7XG5cdFx0XHR0aGlzLnN1Y2Nlc3MgPSBmYWxzZTtcblx0XHRcdCg8YW55PnRoaXMpLm1lc3NhZ2UgPSBtZXNzYWdlO1xuXHRcdH0gZWxzZSB7XG5cdFx0XHR0aGlzLnN1Y2Nlc3MgPSB0cnVlO1xuXHRcdH1cblx0fVxufVxuXG5leHBvcnQgY2xhc3MgRXZlbnQgZXh0ZW5kcyBNZXNzYWdlIGltcGxlbWVudHMgRGVidWdQcm90b2NvbC5FdmVudCB7XG5cdGV2ZW50OiBzdHJpbmc7XG5cblx0cHVibGljIGNvbnN0cnVjdG9yKGV2ZW50OiBzdHJpbmcsIGJvZHk/OiBhbnkpIHtcblx0XHRzdXBlcignZXZlbnQnKTtcblx0XHR0aGlzLmV2ZW50ID0gZXZlbnQ7XG5cdFx0aWYgKGJvZHkpIHtcblx0XHRcdCg8YW55PnRoaXMpLmJvZHkgPSBib2R5O1xuXHRcdH1cblx0fVxufVxuIl19

/***/ }),

/***/ 48:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.trimLastNewline = exports.LogOutputEvent = exports.logger = exports.Logger = exports.LogLevel = void 0;
const internalLogger_1 = __webpack_require__(135);
const debugSession_1 = __webpack_require__(46);
var LogLevel;
(function (LogLevel) {
    LogLevel[LogLevel["Verbose"] = 0] = "Verbose";
    LogLevel[LogLevel["Log"] = 1] = "Log";
    LogLevel[LogLevel["Warn"] = 2] = "Warn";
    LogLevel[LogLevel["Error"] = 3] = "Error";
    LogLevel[LogLevel["Stop"] = 4] = "Stop";
})(LogLevel = exports.LogLevel || (exports.LogLevel = {}));
class Logger {
    constructor() {
        this._pendingLogQ = [];
    }
    log(msg, level = LogLevel.Log) {
        msg = msg + '\n';
        this._write(msg, level);
    }
    verbose(msg) {
        this.log(msg, LogLevel.Verbose);
    }
    warn(msg) {
        this.log(msg, LogLevel.Warn);
    }
    error(msg) {
        this.log(msg, LogLevel.Error);
    }
    dispose() {
        if (this._currentLogger) {
            const disposeP = this._currentLogger.dispose();
            this._currentLogger = null;
            return disposeP;
        }
        else {
            return Promise.resolve();
        }
    }
    /**
     * `log` adds a newline, `write` doesn't
     */
    _write(msg, level = LogLevel.Log) {
        // [null, undefined] => string
        msg = msg + '';
        if (this._pendingLogQ) {
            this._pendingLogQ.push({ msg, level });
        }
        else if (this._currentLogger) {
            this._currentLogger.log(msg, level);
        }
    }
    /**
     * Set the logger's minimum level to log in the console, and whether to log to the file. Log messages are queued before this is
     * called the first time, because minLogLevel defaults to Warn.
     */
    setup(consoleMinLogLevel, _logFilePath, prependTimestamp = true) {
        const logFilePath = typeof _logFilePath === 'string' ?
            _logFilePath :
            (_logFilePath && this._logFilePathFromInit);
        if (this._currentLogger) {
            const options = {
                consoleMinLogLevel,
                logFilePath,
                prependTimestamp
            };
            this._currentLogger.setup(options).then(() => {
                // Now that we have a minimum logLevel, we can clear out the queue of pending messages
                if (this._pendingLogQ) {
                    const logQ = this._pendingLogQ;
                    this._pendingLogQ = null;
                    logQ.forEach(item => this._write(item.msg, item.level));
                }
            });
        }
    }
    init(logCallback, logFilePath, logToConsole) {
        // Re-init, create new global Logger
        this._pendingLogQ = this._pendingLogQ || [];
        this._currentLogger = new internalLogger_1.InternalLogger(logCallback, logToConsole);
        this._logFilePathFromInit = logFilePath;
    }
}
exports.Logger = Logger;
exports.logger = new Logger();
class LogOutputEvent extends debugSession_1.OutputEvent {
    constructor(msg, level) {
        const category = level === LogLevel.Error ? 'stderr' :
            level === LogLevel.Warn ? 'console' :
                'stdout';
        super(msg, category);
    }
}
exports.LogOutputEvent = LogOutputEvent;
function trimLastNewline(str) {
    return str.replace(/(\n|\r\n)$/, '');
}
exports.trimLastNewline = trimLastNewline;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibG9nZ2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vc3JjL2xvZ2dlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUE7OzREQUU0RDs7O0FBRTVELHFEQUFrRDtBQUNsRCxpREFBNkM7QUFFN0MsSUFBWSxRQU1YO0FBTkQsV0FBWSxRQUFRO0lBQ25CLDZDQUFXLENBQUE7SUFDWCxxQ0FBTyxDQUFBO0lBQ1AsdUNBQVEsQ0FBQTtJQUNSLHlDQUFTLENBQUE7SUFDVCx1Q0FBUSxDQUFBO0FBQ1QsQ0FBQyxFQU5XLFFBQVEsR0FBUixnQkFBUSxLQUFSLGdCQUFRLFFBTW5CO0FBNEJELE1BQWEsTUFBTTtJQUFuQjtRQUlTLGlCQUFZLEdBQWUsRUFBRSxDQUFDO0lBMkV2QyxDQUFDO0lBekVBLEdBQUcsQ0FBQyxHQUFXLEVBQUUsS0FBSyxHQUFHLFFBQVEsQ0FBQyxHQUFHO1FBQ3BDLEdBQUcsR0FBRyxHQUFHLEdBQUcsSUFBSSxDQUFDO1FBQ2pCLElBQUksQ0FBQyxNQUFNLENBQUMsR0FBRyxFQUFFLEtBQUssQ0FBQyxDQUFDO0lBQ3pCLENBQUM7SUFFRCxPQUFPLENBQUMsR0FBVztRQUNsQixJQUFJLENBQUMsR0FBRyxDQUFDLEdBQUcsRUFBRSxRQUFRLENBQUMsT0FBTyxDQUFDLENBQUM7SUFDakMsQ0FBQztJQUVELElBQUksQ0FBQyxHQUFXO1FBQ2YsSUFBSSxDQUFDLEdBQUcsQ0FBQyxHQUFHLEVBQUUsUUFBUSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQzlCLENBQUM7SUFFRCxLQUFLLENBQUMsR0FBVztRQUNoQixJQUFJLENBQUMsR0FBRyxDQUFDLEdBQUcsRUFBRSxRQUFRLENBQUMsS0FBSyxDQUFDLENBQUM7SUFDL0IsQ0FBQztJQUVELE9BQU87UUFDTixJQUFJLElBQUksQ0FBQyxjQUFjLEVBQUU7WUFDeEIsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLGNBQWMsQ0FBQyxPQUFPLEVBQUUsQ0FBQztZQUMvQyxJQUFJLENBQUMsY0FBYyxHQUFHLElBQUksQ0FBQztZQUMzQixPQUFPLFFBQVEsQ0FBQztTQUNoQjthQUFNO1lBQ04sT0FBTyxPQUFPLENBQUMsT0FBTyxFQUFFLENBQUM7U0FDekI7SUFDRixDQUFDO0lBRUQ7O09BRUc7SUFDSyxNQUFNLENBQUMsR0FBVyxFQUFFLEtBQUssR0FBRyxRQUFRLENBQUMsR0FBRztRQUMvQyw4QkFBOEI7UUFDOUIsR0FBRyxHQUFHLEdBQUcsR0FBRyxFQUFFLENBQUM7UUFDZixJQUFJLElBQUksQ0FBQyxZQUFZLEVBQUU7WUFDdEIsSUFBSSxDQUFDLFlBQVksQ0FBQyxJQUFJLENBQUMsRUFBRSxHQUFHLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQztTQUN2QzthQUFNLElBQUksSUFBSSxDQUFDLGNBQWMsRUFBRTtZQUMvQixJQUFJLENBQUMsY0FBYyxDQUFDLEdBQUcsQ0FBQyxHQUFHLEVBQUUsS0FBSyxDQUFDLENBQUM7U0FDcEM7SUFDRixDQUFDO0lBRUQ7OztPQUdHO0lBQ0gsS0FBSyxDQUFDLGtCQUE0QixFQUFFLFlBQTZCLEVBQUUsbUJBQTRCLElBQUk7UUFDbEcsTUFBTSxXQUFXLEdBQUcsT0FBTyxZQUFZLEtBQUssUUFBUSxDQUFDLENBQUM7WUFDckQsWUFBWSxDQUFDLENBQUM7WUFDZCxDQUFDLFlBQVksSUFBSSxJQUFJLENBQUMsb0JBQW9CLENBQUMsQ0FBQztRQUU3QyxJQUFJLElBQUksQ0FBQyxjQUFjLEVBQUU7WUFDeEIsTUFBTSxPQUFPLEdBQUc7Z0JBQ2Ysa0JBQWtCO2dCQUNsQixXQUFXO2dCQUNYLGdCQUFnQjthQUNoQixDQUFDO1lBQ0YsSUFBSSxDQUFDLGNBQWMsQ0FBQyxLQUFLLENBQUMsT0FBTyxDQUFDLENBQUMsSUFBSSxDQUFDLEdBQUcsRUFBRTtnQkFDNUMsc0ZBQXNGO2dCQUN0RixJQUFJLElBQUksQ0FBQyxZQUFZLEVBQUU7b0JBQ3RCLE1BQU0sSUFBSSxHQUFHLElBQUksQ0FBQyxZQUFZLENBQUM7b0JBQy9CLElBQUksQ0FBQyxZQUFZLEdBQUcsSUFBSSxDQUFDO29CQUN6QixJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsR0FBRyxFQUFFLElBQUksQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFDO2lCQUN4RDtZQUNGLENBQUMsQ0FBQyxDQUFDO1NBRUg7SUFDRixDQUFDO0lBRUQsSUFBSSxDQUFDLFdBQXlCLEVBQUUsV0FBb0IsRUFBRSxZQUFzQjtRQUMzRSxvQ0FBb0M7UUFDcEMsSUFBSSxDQUFDLFlBQVksR0FBRyxJQUFJLENBQUMsWUFBWSxJQUFJLEVBQUUsQ0FBQztRQUM1QyxJQUFJLENBQUMsY0FBYyxHQUFHLElBQUksK0JBQWMsQ0FBQyxXQUFXLEVBQUUsWUFBWSxDQUFDLENBQUM7UUFDcEUsSUFBSSxDQUFDLG9CQUFvQixHQUFHLFdBQVcsQ0FBQztJQUN6QyxDQUFDO0NBQ0Q7QUEvRUQsd0JBK0VDO0FBRVksUUFBQSxNQUFNLEdBQUcsSUFBSSxNQUFNLEVBQUUsQ0FBQztBQUVuQyxNQUFhLGNBQWUsU0FBUSwwQkFBVztJQUM5QyxZQUFZLEdBQVcsRUFBRSxLQUFlO1FBQ3ZDLE1BQU0sUUFBUSxHQUNiLEtBQUssS0FBSyxRQUFRLENBQUMsS0FBSyxDQUFDLENBQUMsQ0FBQyxRQUFRLENBQUMsQ0FBQztZQUNyQyxLQUFLLEtBQUssUUFBUSxDQUFDLElBQUksQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLENBQUM7Z0JBQ3JDLFFBQVEsQ0FBQztRQUNWLEtBQUssQ0FBQyxHQUFHLEVBQUUsUUFBUSxDQUFDLENBQUM7SUFDdEIsQ0FBQztDQUNEO0FBUkQsd0NBUUM7QUFFRCxTQUFnQixlQUFlLENBQUMsR0FBVztJQUMxQyxPQUFPLEdBQUcsQ0FBQyxPQUFPLENBQUMsWUFBWSxFQUFFLEVBQUUsQ0FBQyxDQUFDO0FBQ3RDLENBQUM7QUFGRCwwQ0FFQyIsInNvdXJjZXNDb250ZW50IjpbIi8qLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4gKiBDb3B5cmlnaHQgKEMpIE1pY3Jvc29mdCBDb3Jwb3JhdGlvbi4gQWxsIHJpZ2h0cyByZXNlcnZlZC5cbiAqLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0qL1xuXG5pbXBvcnQgeyBJbnRlcm5hbExvZ2dlciB9IGZyb20gJy4vaW50ZXJuYWxMb2dnZXInO1xuaW1wb3J0IHsgT3V0cHV0RXZlbnQgfSBmcm9tICcuL2RlYnVnU2Vzc2lvbic7XG5cbmV4cG9ydCBlbnVtIExvZ0xldmVsIHtcblx0VmVyYm9zZSA9IDAsXG5cdExvZyA9IDEsXG5cdFdhcm4gPSAyLFxuXHRFcnJvciA9IDMsXG5cdFN0b3AgPSA0XG59XG5cbmV4cG9ydCB0eXBlIElMb2dDYWxsYmFjayA9IChvdXRwdXRFdmVudDogT3V0cHV0RXZlbnQpID0+IHZvaWQ7XG5cbmludGVyZmFjZSBJTG9nSXRlbSB7XG5cdG1zZzogc3RyaW5nO1xuXHRsZXZlbDogTG9nTGV2ZWw7XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgSUxvZ2dlciB7XG5cdGxvZyhtc2c6IHN0cmluZywgbGV2ZWw/OiBMb2dMZXZlbCk6IHZvaWQ7XG5cdHZlcmJvc2UobXNnOiBzdHJpbmcpOiB2b2lkO1xuXHR3YXJuKG1zZzogc3RyaW5nKTogdm9pZDtcblx0ZXJyb3IobXNnOiBzdHJpbmcpOiB2b2lkO1xufVxuXG5leHBvcnQgaW50ZXJmYWNlIElJbnRlcm5hbExvZ2dlciB7XG5cdGRpc3Bvc2UoKTogUHJvbWlzZTx2b2lkPjtcblx0bG9nKG1zZzogc3RyaW5nLCBsZXZlbDogTG9nTGV2ZWwsIHByZXBlbmRUaW1lc3RhbXA/OiBib29sZWFuKSA6IHZvaWQ7XG5cdHNldHVwKG9wdGlvbnM6IElJbnRlcm5hbExvZ2dlck9wdGlvbnMpOiBQcm9taXNlPHZvaWQ+O1xufVxuXG5leHBvcnQgaW50ZXJmYWNlIElJbnRlcm5hbExvZ2dlck9wdGlvbnMge1xuXHRjb25zb2xlTWluTG9nTGV2ZWw6IExvZ0xldmVsO1xuXHRsb2dGaWxlUGF0aD86IHN0cmluZztcblx0cHJlcGVuZFRpbWVzdGFtcD86IGJvb2xlYW47XG59XG5cbmV4cG9ydCBjbGFzcyBMb2dnZXIge1xuXHRwcml2YXRlIF9sb2dGaWxlUGF0aEZyb21Jbml0OiBzdHJpbmc7XG5cblx0cHJpdmF0ZSBfY3VycmVudExvZ2dlcjogSUludGVybmFsTG9nZ2VyO1xuXHRwcml2YXRlIF9wZW5kaW5nTG9nUTogSUxvZ0l0ZW1bXSA9IFtdO1xuXG5cdGxvZyhtc2c6IHN0cmluZywgbGV2ZWwgPSBMb2dMZXZlbC5Mb2cpOiB2b2lkIHtcblx0XHRtc2cgPSBtc2cgKyAnXFxuJztcblx0XHR0aGlzLl93cml0ZShtc2csIGxldmVsKTtcblx0fVxuXG5cdHZlcmJvc2UobXNnOiBzdHJpbmcpOiB2b2lkIHtcblx0XHR0aGlzLmxvZyhtc2csIExvZ0xldmVsLlZlcmJvc2UpO1xuXHR9XG5cblx0d2Fybihtc2c6IHN0cmluZyk6IHZvaWQge1xuXHRcdHRoaXMubG9nKG1zZywgTG9nTGV2ZWwuV2Fybik7XG5cdH1cblxuXHRlcnJvcihtc2c6IHN0cmluZyk6IHZvaWQge1xuXHRcdHRoaXMubG9nKG1zZywgTG9nTGV2ZWwuRXJyb3IpO1xuXHR9XG5cblx0ZGlzcG9zZSgpOiBQcm9taXNlPHZvaWQ+IHtcblx0XHRpZiAodGhpcy5fY3VycmVudExvZ2dlcikge1xuXHRcdFx0Y29uc3QgZGlzcG9zZVAgPSB0aGlzLl9jdXJyZW50TG9nZ2VyLmRpc3Bvc2UoKTtcblx0XHRcdHRoaXMuX2N1cnJlbnRMb2dnZXIgPSBudWxsO1xuXHRcdFx0cmV0dXJuIGRpc3Bvc2VQO1xuXHRcdH0gZWxzZSB7XG5cdFx0XHRyZXR1cm4gUHJvbWlzZS5yZXNvbHZlKCk7XG5cdFx0fVxuXHR9XG5cblx0LyoqXG5cdCAqIGBsb2dgIGFkZHMgYSBuZXdsaW5lLCBgd3JpdGVgIGRvZXNuJ3Rcblx0ICovXG5cdHByaXZhdGUgX3dyaXRlKG1zZzogc3RyaW5nLCBsZXZlbCA9IExvZ0xldmVsLkxvZyk6IHZvaWQge1xuXHRcdC8vIFtudWxsLCB1bmRlZmluZWRdID0+IHN0cmluZ1xuXHRcdG1zZyA9IG1zZyArICcnO1xuXHRcdGlmICh0aGlzLl9wZW5kaW5nTG9nUSkge1xuXHRcdFx0dGhpcy5fcGVuZGluZ0xvZ1EucHVzaCh7IG1zZywgbGV2ZWwgfSk7XG5cdFx0fSBlbHNlIGlmICh0aGlzLl9jdXJyZW50TG9nZ2VyKSB7XG5cdFx0XHR0aGlzLl9jdXJyZW50TG9nZ2VyLmxvZyhtc2csIGxldmVsKTtcblx0XHR9XG5cdH1cblxuXHQvKipcblx0ICogU2V0IHRoZSBsb2dnZXIncyBtaW5pbXVtIGxldmVsIHRvIGxvZyBpbiB0aGUgY29uc29sZSwgYW5kIHdoZXRoZXIgdG8gbG9nIHRvIHRoZSBmaWxlLiBMb2cgbWVzc2FnZXMgYXJlIHF1ZXVlZCBiZWZvcmUgdGhpcyBpc1xuXHQgKiBjYWxsZWQgdGhlIGZpcnN0IHRpbWUsIGJlY2F1c2UgbWluTG9nTGV2ZWwgZGVmYXVsdHMgdG8gV2Fybi5cblx0ICovXG5cdHNldHVwKGNvbnNvbGVNaW5Mb2dMZXZlbDogTG9nTGV2ZWwsIF9sb2dGaWxlUGF0aD86IHN0cmluZ3xib29sZWFuLCBwcmVwZW5kVGltZXN0YW1wOiBib29sZWFuID0gdHJ1ZSk6IHZvaWQge1xuXHRcdGNvbnN0IGxvZ0ZpbGVQYXRoID0gdHlwZW9mIF9sb2dGaWxlUGF0aCA9PT0gJ3N0cmluZycgP1xuXHRcdFx0X2xvZ0ZpbGVQYXRoIDpcblx0XHRcdChfbG9nRmlsZVBhdGggJiYgdGhpcy5fbG9nRmlsZVBhdGhGcm9tSW5pdCk7XG5cblx0XHRpZiAodGhpcy5fY3VycmVudExvZ2dlcikge1xuXHRcdFx0Y29uc3Qgb3B0aW9ucyA9IHtcblx0XHRcdFx0Y29uc29sZU1pbkxvZ0xldmVsLFxuXHRcdFx0XHRsb2dGaWxlUGF0aCxcblx0XHRcdFx0cHJlcGVuZFRpbWVzdGFtcFxuXHRcdFx0fTtcblx0XHRcdHRoaXMuX2N1cnJlbnRMb2dnZXIuc2V0dXAob3B0aW9ucykudGhlbigoKSA9PiB7XG5cdFx0XHRcdC8vIE5vdyB0aGF0IHdlIGhhdmUgYSBtaW5pbXVtIGxvZ0xldmVsLCB3ZSBjYW4gY2xlYXIgb3V0IHRoZSBxdWV1ZSBvZiBwZW5kaW5nIG1lc3NhZ2VzXG5cdFx0XHRcdGlmICh0aGlzLl9wZW5kaW5nTG9nUSkge1xuXHRcdFx0XHRcdGNvbnN0IGxvZ1EgPSB0aGlzLl9wZW5kaW5nTG9nUTtcblx0XHRcdFx0XHR0aGlzLl9wZW5kaW5nTG9nUSA9IG51bGw7XG5cdFx0XHRcdFx0bG9nUS5mb3JFYWNoKGl0ZW0gPT4gdGhpcy5fd3JpdGUoaXRlbS5tc2csIGl0ZW0ubGV2ZWwpKTtcblx0XHRcdFx0fVxuXHRcdFx0fSk7XG5cblx0XHR9XG5cdH1cblxuXHRpbml0KGxvZ0NhbGxiYWNrOiBJTG9nQ2FsbGJhY2ssIGxvZ0ZpbGVQYXRoPzogc3RyaW5nLCBsb2dUb0NvbnNvbGU/OiBib29sZWFuKTogdm9pZCB7XG5cdFx0Ly8gUmUtaW5pdCwgY3JlYXRlIG5ldyBnbG9iYWwgTG9nZ2VyXG5cdFx0dGhpcy5fcGVuZGluZ0xvZ1EgPSB0aGlzLl9wZW5kaW5nTG9nUSB8fCBbXTtcblx0XHR0aGlzLl9jdXJyZW50TG9nZ2VyID0gbmV3IEludGVybmFsTG9nZ2VyKGxvZ0NhbGxiYWNrLCBsb2dUb0NvbnNvbGUpO1xuXHRcdHRoaXMuX2xvZ0ZpbGVQYXRoRnJvbUluaXQgPSBsb2dGaWxlUGF0aDtcblx0fVxufVxuXG5leHBvcnQgY29uc3QgbG9nZ2VyID0gbmV3IExvZ2dlcigpO1xuXG5leHBvcnQgY2xhc3MgTG9nT3V0cHV0RXZlbnQgZXh0ZW5kcyBPdXRwdXRFdmVudCB7XG5cdGNvbnN0cnVjdG9yKG1zZzogc3RyaW5nLCBsZXZlbDogTG9nTGV2ZWwpIHtcblx0XHRjb25zdCBjYXRlZ29yeSA9XG5cdFx0XHRsZXZlbCA9PT0gTG9nTGV2ZWwuRXJyb3IgPyAnc3RkZXJyJyA6XG5cdFx0XHRsZXZlbCA9PT0gTG9nTGV2ZWwuV2FybiA/ICdjb25zb2xlJyA6XG5cdFx0XHQnc3Rkb3V0Jztcblx0XHRzdXBlcihtc2csIGNhdGVnb3J5KTtcblx0fVxufVxuXG5leHBvcnQgZnVuY3Rpb24gdHJpbUxhc3ROZXdsaW5lKHN0cjogc3RyaW5nKTogc3RyaW5nIHtcblx0cmV0dXJuIHN0ci5yZXBsYWNlKC8oXFxufFxcclxcbikkLywgJycpO1xufVxuXG5cbiJdfQ==

/***/ }),

/***/ 6:
/***/ (function(module, exports) {

module.exports = require("fs");

/***/ }),

/***/ 682:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------
 * Copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
// This file is for running the godlvdap debug adapter as a standalone program
// in a separate process (e.g. when working in --server mode).
//
// NOTE: we must not include this file when we switch to the inline debug adapter
// launch mode. This installs a process-wide uncaughtException handler
// which can result in the extension host crash.
const vscode_debugadapter_1 = __webpack_require__(90);
const goDlvDebug_1 = __webpack_require__(683);
process.on('uncaughtException', (err) => {
    const errMessage = err && (err.stack || err.message);
    vscode_debugadapter_1.logger.error(`Unhandled error in debug adapter: ${errMessage}`);
    throw err;
});
goDlvDebug_1.GoDlvDapDebugSession.run(goDlvDebug_1.GoDlvDapDebugSession);


/***/ }),

/***/ 683:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

/*---------------------------------------------------------
 * Copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
exports.GoDlvDapDebugSession = void 0;
// NOTE: This debug adapter is experimental, in-development code. If you
// actually need to debug Go code, please use the default adapter.
const child_process_1 = __webpack_require__(4);
const fs = __webpack_require__(6);
const net = __webpack_require__(26);
const os = __webpack_require__(29);
const path = __webpack_require__(2);
const vscode_debugadapter_1 = __webpack_require__(90);
const pathUtils_1 = __webpack_require__(12);
const processUtils_1 = __webpack_require__(24);
const dapClient_1 = __webpack_require__(684);
function logArgsToString(args) {
    return args
        .map((arg) => {
        return typeof arg === 'string' ? arg : JSON.stringify(arg);
    })
        .join(' ');
}
function log(...args) {
    vscode_debugadapter_1.logger.warn(logArgsToString(args));
}
function logError(...args) {
    vscode_debugadapter_1.logger.error(logArgsToString(args));
}
// GoDlvDapDebugSession implements a DAP debug adapter to talk to the editor.
//
// This adapter serves as a DAP proxy between the editor and the DAP server
// inside Delve. It relies on functionality inherited from DebugSession to
// implement the server side interfacing the editor, and on DapClient to
// implement the client side interfacing Delve:
//
//      Editor                GoDlvDapDebugSession                 Delve
//  +------------+        +--------------+-----------+         +------------+
//  | DAP Client | <====> | DebugSession | DAPClient |  <====> | DAP Server |
//  +------------+        +--------------+-----------+         +------------+
class GoDlvDapDebugSession extends vscode_debugadapter_1.LoggingDebugSession {
    constructor() {
        super();
        this.DEFAULT_DELVE_HOST = '127.0.0.1';
        this.DEFAULT_DELVE_PORT = 42042;
        this.logLevel = vscode_debugadapter_1.Logger.LogLevel.Error;
        this.dlvClient = null;
        // Child process used to track debugee launched without debugging (noDebug
        // mode). Either debugProcess or dlvClient are null.
        this.debugProcess = null;
        // Invoke logger.init here because we want logging to work in 'inline'
        // DA mode. It's typically called in the start() method of our parent
        // class, but this method isn't called in 'inline' mode.
        vscode_debugadapter_1.logger.init((e) => this.sendEvent(e));
        // this debugger uses zero-based lines and columns
        this.setDebuggerLinesStartAt1(false);
        this.setDebuggerColumnsStartAt1(false);
    }
    initializeRequest(response, args, request) {
        log('InitializeRequest');
        response.body.supportsConfigurationDoneRequest = true;
        // We respond to InitializeRequest here, because Delve hasn't been
        // launched yet. Delve will start responding to DAP requests after
        // LaunchRequest is received, which tell us how to start it.
        // TODO: we could send an InitializeRequest to Delve when
        // it launches, wait for its response and sanity check the capabilities
        // it reports. Once DAP support in Delve is complete, this can be part
        // of making sure that the "dlv" binary we find is sufficiently
        // up-to-date to talk DAP with us.
        this.sendResponse(response);
        log('InitializeResponse');
    }
    launchRequest(response, args, request) {
        // Setup logger now that we have the 'trace' level passed in from
        // LaunchRequestArguments.
        this.logLevel =
            args.trace === 'verbose'
                ? vscode_debugadapter_1.Logger.LogLevel.Verbose
                : args.trace === 'log'
                    ? vscode_debugadapter_1.Logger.LogLevel.Log
                    : vscode_debugadapter_1.Logger.LogLevel.Error;
        const logPath = this.logLevel !== vscode_debugadapter_1.Logger.LogLevel.Error ? path.join(os.tmpdir(), 'vscode-godlvdapdebug.txt') : undefined;
        vscode_debugadapter_1.logger.setup(this.logLevel, logPath);
        log('launchRequest');
        // In noDebug mode with the 'debug' launch mode, we don't launch Delve
        // but run the debugee directly.
        // For other launch modes we currently still defer to Delve, for
        // compatibility with the old debugAdapter.
        // See https://github.com/golang/vscode-go/issues/336
        if (args.noDebug && args.mode === 'debug') {
            try {
                this.launchNoDebug(args);
            }
            catch (e) {
                logError(`launchNoDebug failed: "${e}"`);
                // TODO: define error constants
                // https://github.com/golang/vscode-go/issues/305
                this.sendErrorResponse(response, 3000, `Failed to launch "${e}"`);
            }
            return;
        }
        if (!args.port) {
            args.port = this.DEFAULT_DELVE_PORT;
        }
        if (!args.host) {
            args.host = this.DEFAULT_DELVE_HOST;
        }
        this.dlvClient = new DelveClient(args);
        this.dlvClient.on('stdout', (str) => {
            log('dlv stdout:', str);
        });
        this.dlvClient.on('stderr', (str) => {
            log('dlv stderr:', str);
        });
        this.dlvClient.on('connected', () => {
            // Once the client is connected to Delve, forward it the launch
            // request to begin the actual debugging session.
            this.dlvClient.send(request);
        });
        this.dlvClient.on('close', (rc) => {
            if (rc !== 0) {
                // TODO: define error constants
                // https://github.com/golang/vscode-go/issues/305
                this.sendErrorResponse(response, 3000, 'Failed to continue: Check the debug console for details.');
            }
            log('Sending TerminatedEvent as delve is closed');
            this.sendEvent(new vscode_debugadapter_1.TerminatedEvent());
        });
        // Relay events and responses back to vscode. In the future we will
        // add middleware here to intercept specific kinds of responses/events
        // for special handling.
        this.dlvClient.on('event', (event) => {
            this.sendEvent(event);
        });
        this.dlvClient.on('response', (resp) => {
            this.sendResponse(resp);
        });
    }
    attachRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    disconnectRequest(response, args, request) {
        log('DisconnectRequest');
        // How we handle DisconnectRequest depends on whether Delve was launched
        // at all.
        // * In noDebug node, the Go program was spawned directly without
        //   debugging: this.debugProcess will be non-null, and this.dlvClient
        //   will be null.
        // * Otherwise, Delve was spawned: this.debugProcess will be null, and
        //   this.dlvClient will be non-null.
        if (this.debugProcess !== null) {
            log(`killing debugee (pid: ${this.debugProcess.pid})...`);
            // Kill the debugee and notify the client when the killing is
            // completed, to ensure a clean shutdown sequence.
            processUtils_1.killProcessTree(this.debugProcess, log).then(() => {
                super.disconnectRequest(response, args);
                log('DisconnectResponse');
            });
        }
        else if (this.dlvClient !== null) {
            // Forward this DisconnectRequest to Delve.
            this.dlvClient.send(request);
        }
        else {
            logError(`both debug process and dlv client are null`);
            // TODO: define all error codes as constants
            // https://github.com/golang/vscode-go/issues/305
            this.sendErrorResponse(response, 3000, 'Failed to disconnect: Check the debug console for details.');
        }
    }
    terminateRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    restartRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setBreakPointsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setFunctionBreakPointsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setExceptionBreakPointsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    configurationDoneRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    continueRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    nextRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    stepInRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    stepOutRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    stepBackRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    reverseContinueRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    restartFrameRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    gotoRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    pauseRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    sourceRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    threadsRequest(response, request) {
        this.dlvClient.send(request);
    }
    terminateThreadsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    stackTraceRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    scopesRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    variablesRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setVariableRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setExpressionRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    evaluateRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    stepInTargetsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    gotoTargetsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    completionsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    exceptionInfoRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    loadedSourcesRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    dataBreakpointInfoRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setDataBreakpointsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    readMemoryRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    disassembleRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    cancelRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    breakpointLocationsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    setInstructionBreakpointsRequest(response, args, request) {
        this.dlvClient.send(request);
    }
    // Launch the debugee process without starting a debugger.
    // This implements the `Run > Run Without Debugger` functionality in vscode.
    // Note: this method currently assumes launchArgs.mode === 'debug'.
    launchNoDebug(launchArgs) {
        if (launchArgs.mode !== 'debug') {
            throw new Error('launchNoDebug requires "debug" mode');
        }
        const { program, dirname, programIsDirectory } = parseProgramArgSync(launchArgs);
        const goRunArgs = ['run'];
        if (launchArgs.buildFlags) {
            goRunArgs.push(launchArgs.buildFlags);
        }
        if (programIsDirectory) {
            goRunArgs.push('.');
        }
        else {
            goRunArgs.push(program);
        }
        if (launchArgs.args) {
            goRunArgs.push(...launchArgs.args);
        }
        // launchArgs.env includes all the environment variables
        // including vscode-go's toolsExecutionEnvironment (PATH, GOPATH, ...),
        // and those read from .env files.
        const launchArgsEnv = launchArgs.env || {};
        const programEnv = Object.assign({}, process.env, launchArgsEnv);
        log(`Current working directory: ${dirname}`);
        const goExe = pathUtils_1.getBinPathWithPreferredGopathGoroot('go', []);
        log(`Running: ${goExe} ${goRunArgs.join(' ')}`);
        this.debugProcess = child_process_1.spawn(goExe, goRunArgs, {
            cwd: dirname,
            env: programEnv
        });
        this.debugProcess.stderr.on('data', (str) => {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent(str.toString(), 'stderr'));
        });
        this.debugProcess.stdout.on('data', (str) => {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent(str.toString(), 'stdout'));
        });
        this.debugProcess.on('close', (rc) => {
            this.sendEvent(new vscode_debugadapter_1.TerminatedEvent());
        });
    }
}
exports.GoDlvDapDebugSession = GoDlvDapDebugSession;
// DelveClient provides a DAP client to talk to a DAP server in Delve.
//
// After creation, it emits the following events:
//
//    'connected':            client is connected to delve
//    'request (request)':    delve sent request
//    'response (response)':  delve sent response
//    'event (event)':        delve sent event
//    'stdout' (str):         delve emitted str to stdout
//    'stderr' (str):         delve emitted str to stderr
//    'close' (rc):           delve exited with return code rc
class DelveClient extends dapClient_1.DAPClient {
    constructor(launchArgs) {
        super();
        this.serverStarted = false;
        const launchArgsEnv = launchArgs.env || {};
        const env = Object.assign({}, process.env, launchArgsEnv);
        // Let users override direct path to delve by setting it in the env
        // map in launch.json; if unspecified, fall back to dlvToolPath.
        let dlvPath = launchArgsEnv['dlvPath'];
        if (!dlvPath) {
            dlvPath = launchArgs.dlvToolPath;
        }
        if (!fs.existsSync(dlvPath)) {
            log(`Couldn't find dlv at the Go tools path, ${process.env['GOPATH']}${env['GOPATH'] ? ', ' + env['GOPATH'] : ''} or ${pathUtils_1.envPath}`);
            throw new Error(`Cannot find Delve debugger. Install from https://github.com/go-delve/delve/ & ensure it is in your Go tools path, "GOPATH/bin" or "PATH".`);
        }
        const dlvArgs = new Array();
        dlvArgs.push('dap');
        // add user-specified dlv flags first. When duplicate flags are specified,
        // dlv doesn't mind but accepts the last flag value.
        if (launchArgs.dlvFlags && launchArgs.dlvFlags.length > 0) {
            dlvArgs.push(...launchArgs.dlvFlags);
        }
        dlvArgs.push(`--listen=${launchArgs.host}:${launchArgs.port}`);
        if (launchArgs.showLog) {
            dlvArgs.push('--log=' + launchArgs.showLog.toString());
        }
        if (launchArgs.logOutput) {
            dlvArgs.push('--log-output=' + launchArgs.logOutput);
        }
        log(`Running: ${dlvPath} ${dlvArgs.join(' ')}`);
        const dir = parseProgramArgSync(launchArgs).dirname;
        this.debugProcess = child_process_1.spawn(dlvPath, dlvArgs, {
            cwd: dir,
            env
        });
        this.debugProcess.stderr.on('data', (chunk) => {
            let str = chunk.toString();
            str = pathUtils_1.expandFilePathInOutput(str, dir);
            this.emit('stderr', str);
        });
        this.debugProcess.stdout.on('data', (chunk) => {
            const str = chunk.toString();
            this.emit('stdout', str);
            if (!this.serverStarted) {
                this.serverStarted = true;
                this.connectSocketToServer(launchArgs.port, launchArgs.host);
            }
        });
        this.debugProcess.on('close', (rc) => {
            if (rc) {
                logError(`Process exiting with code: ${rc} signal: ${this.debugProcess.killed}`);
            }
            else {
                log(`Process exiting normally ${this.debugProcess.killed}`);
            }
            this.emit('close', rc);
        });
        this.debugProcess.on('error', (err) => {
            throw err;
        });
    }
    // Connect this client to the server. The server is expected to be listening
    // on host:port.
    connectSocketToServer(port, host) {
        // Add a slight delay to ensure that Delve started up the server.
        setTimeout(() => {
            const socket = net.createConnection(port, host, () => {
                this.connect(socket, socket);
                this.emit('connected');
            });
            socket.on('error', (err) => {
                throw err;
            });
        }, 200);
    }
}
// Helper function to parse a program from LaunchRequestArguments. Returns:
// {
//    program: the program arg,
//    dirname: the directory containing the program (or 'program' itself if
//             it's already a directory),
//    programIsDirectory: is the program a directory?
// }
//
// The program argument is taken as-is from launchArgs. If the program path
// is relative, dirname will also be relative. If the program path is absolute,
// dirname will also be absolute.
//
// Throws an exception in case args.program is not a valid file or directory.
// This function can block because it calls a blocking fs function.
function parseProgramArgSync(launchArgs) {
    const program = launchArgs.program;
    if (!program) {
        throw new Error('The program attribute is missing in the debug configuration in launch.json');
    }
    let programIsDirectory = false;
    try {
        programIsDirectory = fs.lstatSync(program).isDirectory();
    }
    catch (e) {
        throw new Error('The program attribute must point to valid directory, .go file or executable.');
    }
    if (!programIsDirectory && path.extname(program) !== '.go') {
        throw new Error('The program attribute must be a directory or .go file in debug mode');
    }
    const dirname = programIsDirectory ? program : path.dirname(program);
    return { program, dirname, programIsDirectory };
}


/***/ }),

/***/ 684:
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
exports.DAPClient = void 0;
/*---------------------------------------------------------
 * Copyright 2020 The Go Authors. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
const events_1 = __webpack_require__(33);
// DapClient implements a simple client for the DAP protocol.
// It's initialized with a pair of streams that the caller creats and enables
// sending and receiving DAP messages over these streams.
// After calling connect():
//
//  - For sending messages call send().
//  - For receiving messages, subscibe to events this class emits.
//      - 'event', 'respones', 'request' - each carrying an appropriate
//        DebugProtocol type as an argument.
class DAPClient extends events_1.EventEmitter {
    constructor() {
        super();
        this.rawData = Buffer.alloc(0);
        this.contentLength = -1;
    }
    send(req) {
        const json = JSON.stringify(req);
        this.outputStream.write(`Content-Length: ${Buffer.byteLength(json, 'utf8')}\r\n\r\n${json}`, 'utf8');
    }
    // Connect this client to a server, which is represented by read and write
    // streams. Before this method is called, send() won't work and no messages
    // from the server will be delivered.
    connect(readable, writable) {
        this.outputStream = writable;
        readable.on('data', (data) => {
            this.handleData(data);
        });
    }
    // Implements parsing of the DAP protocol. We cannot use ProtocolClient
    // from the vscode-debugadapter package, because it's not exported and
    // is not meant for external usage.
    // See https://github.com/microsoft/vscode-debugadapter-node/issues/232
    handleData(data) {
        this.rawData = Buffer.concat([this.rawData, data]);
        while (true) {
            if (this.contentLength >= 0) {
                if (this.rawData.length >= this.contentLength) {
                    const message = this.rawData.toString('utf8', 0, this.contentLength);
                    this.rawData = this.rawData.slice(this.contentLength);
                    this.contentLength = -1;
                    if (message.length > 0) {
                        this.dispatch(message);
                    }
                    continue; // there may be more complete messages to process
                }
            }
            else {
                const idx = this.rawData.indexOf(DAPClient.TWO_CRLF);
                if (idx !== -1) {
                    const header = this.rawData.toString('utf8', 0, idx);
                    const lines = header.split('\r\n');
                    for (const line of lines) {
                        const pair = line.split(/: +/);
                        if (pair[0] === 'Content-Length') {
                            this.contentLength = +pair[1];
                        }
                    }
                    this.rawData = this.rawData.slice(idx + DAPClient.TWO_CRLF.length);
                    continue;
                }
            }
            break;
        }
    }
    dispatch(body) {
        const rawData = JSON.parse(body);
        if (rawData.type === 'event') {
            const event = rawData;
            this.emit('event', event);
        }
        else if (rawData.type === 'response') {
            const response = rawData;
            this.emit('response', response);
        }
        else if (rawData.type === 'request') {
            const request = rawData;
            this.emit('request', request);
        }
        else {
            throw new Error(`unknown message ${JSON.stringify(rawData)}`);
        }
    }
}
exports.DAPClient = DAPClient;
DAPClient.TWO_CRLF = '\r\n\r\n';


/***/ }),

/***/ 73:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var childProcess = __webpack_require__(4);
const { existsSync } = __webpack_require__(6);
var spawn = childProcess.spawn;
var exec = childProcess.exec;

module.exports = function (pid, signal, callback) {
    if (typeof signal === 'function' && callback === undefined) {
        callback = signal;
        signal = undefined;
    }

    pid = parseInt(pid);
    if (Number.isNaN(pid)) {
        if (callback) {
            return callback(new Error("pid must be a number"));
        } else {
            throw new Error("pid must be a number");
        }
    }

    var tree = {};
    var pidsToProcess = {};
    tree[pid] = [];
    pidsToProcess[pid] = 1;

    switch (process.platform) {
    case 'win32':
        exec('taskkill /pid ' + pid + ' /T /F', callback);
        break;
    case 'darwin':
        buildProcessTree(pid, tree, pidsToProcess, function (parentPid) {
            return spawn(pathToPgrep(), ['-P', parentPid]);
        }, function () {
            killAll(tree, signal, callback);
        });
        break;
    // case 'sunos':
    //     buildProcessTreeSunOS(pid, tree, pidsToProcess, function () {
    //         killAll(tree, signal, callback);
    //     });
    //     break;
    default: // Linux
        buildProcessTree(pid, tree, pidsToProcess, function (parentPid) {
          return spawn('ps', ['-o', 'pid', '--no-headers', '--ppid', parentPid]);
        }, function () {
            killAll(tree, signal, callback);
        });
        break;
    }
};

function killAll (tree, signal, callback) {
    var killed = {};
    try {
        Object.keys(tree).forEach(function (pid) {
            tree[pid].forEach(function (pidpid) {
                if (!killed[pidpid]) {
                    killPid(pidpid, signal);
                    killed[pidpid] = 1;
                }
            });
            if (!killed[pid]) {
                killPid(pid, signal);
                killed[pid] = 1;
            }
        });
    } catch (err) {
        if (callback) {
            return callback(err);
        } else {
            throw err;
        }
    }
    if (callback) {
        return callback();
    }
}

function killPid(pid, signal) {
    try {
        process.kill(parseInt(pid, 10), signal);
    }
    catch (err) {
        if (err.code !== 'ESRCH') throw err;
    }
}

function buildProcessTree (parentPid, tree, pidsToProcess, spawnChildProcessesList, cb) {
    var ps = spawnChildProcessesList(parentPid);
    var allData = '';
    ps.stdout.on('data', function (data) {
        var data = data.toString('ascii');
        allData += data;
    });

    var onClose = function (code) {
        delete pidsToProcess[parentPid];

        if (code != 0) {
            // no more parent processes
            if (Object.keys(pidsToProcess).length == 0) {
                cb();
            }
            return;
        }

        allData.match(/\d+/g).forEach(function (pid) {
          pid = parseInt(pid, 10);
          tree[parentPid].push(pid);
          tree[pid] = [];
          pidsToProcess[pid] = 1;
          buildProcessTree(pid, tree, pidsToProcess, spawnChildProcessesList, cb);
        });
    };

    ps.on('close', onClose);
}

var pgrep = '';
function pathToPgrep () {
    if (pgrep) {
        return pgrep;
    }
    // Use the default pgrep, available since os x mountain lion.
    // proctools' pgrep does not implement `-P` correctly and returns
    // unrelated processes.
    // https://github.com/golang/vscode-go/issues/90#issuecomment-634430428
    try {
        pgrep = existsSync('/usr/bin/pgrep') ? '/usr/bin/pgrep' : 'pgrep';
    } catch (e) {
        pgrep = 'pgrep';
    }
    return pgrep;
}

/***/ }),

/***/ 90:
/***/ (function(module, exports, __webpack_require__) {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", { value: true });
exports.Handles = exports.Response = exports.Event = exports.ErrorDestination = exports.CompletionItem = exports.Module = exports.Source = exports.Breakpoint = exports.Variable = exports.Scope = exports.StackFrame = exports.Thread = exports.InvalidatedEvent = exports.ProgressEndEvent = exports.ProgressUpdateEvent = exports.ProgressStartEvent = exports.CapabilitiesEvent = exports.LoadedSourceEvent = exports.ModuleEvent = exports.BreakpointEvent = exports.ThreadEvent = exports.OutputEvent = exports.ContinuedEvent = exports.StoppedEvent = exports.TerminatedEvent = exports.InitializedEvent = exports.logger = exports.Logger = exports.LoggingDebugSession = exports.DebugSession = void 0;
const debugSession_1 = __webpack_require__(46);
Object.defineProperty(exports, "DebugSession", { enumerable: true, get: function () { return debugSession_1.DebugSession; } });
Object.defineProperty(exports, "InitializedEvent", { enumerable: true, get: function () { return debugSession_1.InitializedEvent; } });
Object.defineProperty(exports, "TerminatedEvent", { enumerable: true, get: function () { return debugSession_1.TerminatedEvent; } });
Object.defineProperty(exports, "StoppedEvent", { enumerable: true, get: function () { return debugSession_1.StoppedEvent; } });
Object.defineProperty(exports, "ContinuedEvent", { enumerable: true, get: function () { return debugSession_1.ContinuedEvent; } });
Object.defineProperty(exports, "OutputEvent", { enumerable: true, get: function () { return debugSession_1.OutputEvent; } });
Object.defineProperty(exports, "ThreadEvent", { enumerable: true, get: function () { return debugSession_1.ThreadEvent; } });
Object.defineProperty(exports, "BreakpointEvent", { enumerable: true, get: function () { return debugSession_1.BreakpointEvent; } });
Object.defineProperty(exports, "ModuleEvent", { enumerable: true, get: function () { return debugSession_1.ModuleEvent; } });
Object.defineProperty(exports, "LoadedSourceEvent", { enumerable: true, get: function () { return debugSession_1.LoadedSourceEvent; } });
Object.defineProperty(exports, "CapabilitiesEvent", { enumerable: true, get: function () { return debugSession_1.CapabilitiesEvent; } });
Object.defineProperty(exports, "ProgressStartEvent", { enumerable: true, get: function () { return debugSession_1.ProgressStartEvent; } });
Object.defineProperty(exports, "ProgressUpdateEvent", { enumerable: true, get: function () { return debugSession_1.ProgressUpdateEvent; } });
Object.defineProperty(exports, "ProgressEndEvent", { enumerable: true, get: function () { return debugSession_1.ProgressEndEvent; } });
Object.defineProperty(exports, "InvalidatedEvent", { enumerable: true, get: function () { return debugSession_1.InvalidatedEvent; } });
Object.defineProperty(exports, "Thread", { enumerable: true, get: function () { return debugSession_1.Thread; } });
Object.defineProperty(exports, "StackFrame", { enumerable: true, get: function () { return debugSession_1.StackFrame; } });
Object.defineProperty(exports, "Scope", { enumerable: true, get: function () { return debugSession_1.Scope; } });
Object.defineProperty(exports, "Variable", { enumerable: true, get: function () { return debugSession_1.Variable; } });
Object.defineProperty(exports, "Breakpoint", { enumerable: true, get: function () { return debugSession_1.Breakpoint; } });
Object.defineProperty(exports, "Source", { enumerable: true, get: function () { return debugSession_1.Source; } });
Object.defineProperty(exports, "Module", { enumerable: true, get: function () { return debugSession_1.Module; } });
Object.defineProperty(exports, "CompletionItem", { enumerable: true, get: function () { return debugSession_1.CompletionItem; } });
Object.defineProperty(exports, "ErrorDestination", { enumerable: true, get: function () { return debugSession_1.ErrorDestination; } });
const loggingDebugSession_1 = __webpack_require__(134);
Object.defineProperty(exports, "LoggingDebugSession", { enumerable: true, get: function () { return loggingDebugSession_1.LoggingDebugSession; } });
const Logger = __webpack_require__(48);
exports.Logger = Logger;
const messages_1 = __webpack_require__(47);
Object.defineProperty(exports, "Event", { enumerable: true, get: function () { return messages_1.Event; } });
Object.defineProperty(exports, "Response", { enumerable: true, get: function () { return messages_1.Response; } });
const handles_1 = __webpack_require__(137);
Object.defineProperty(exports, "Handles", { enumerable: true, get: function () { return handles_1.Handles; } });
const logger = Logger.logger;
exports.logger = logger;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWFpbi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uL3NyYy9tYWluLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Z0dBR2dHO0FBQ2hHLFlBQVksQ0FBQzs7O0FBRWIsaURBT3dCO0FBU3ZCLDZGQWZBLDJCQUFZLE9BZUE7QUFJWixpR0FsQkEsK0JBQWdCLE9Ba0JBO0FBQUUsZ0dBbEJBLDhCQUFlLE9Ba0JBO0FBQUUsNkZBbEJBLDJCQUFZLE9Ba0JBO0FBQUUsK0ZBbEJBLDZCQUFjLE9Ba0JBO0FBQUUsNEZBbEJBLDBCQUFXLE9Ba0JBO0FBQUUsNEZBbEJBLDBCQUFXLE9Ba0JBO0FBQUUsZ0dBbEJBLDhCQUFlLE9Ba0JBO0FBQUUsNEZBbEJBLDBCQUFXLE9Ba0JBO0FBQ3RILGtHQWxCQSxnQ0FBaUIsT0FrQkE7QUFBRSxrR0FsQkEsZ0NBQWlCLE9Ba0JBO0FBQUUsbUdBbEJBLGlDQUFrQixPQWtCQTtBQUFFLG9HQWxCQSxrQ0FBbUIsT0FrQkE7QUFBRSxpR0FsQkEsK0JBQWdCLE9Ba0JBO0FBQUUsaUdBbEJBLCtCQUFnQixPQWtCQTtBQUNsSCx1RkFsQkEscUJBQU0sT0FrQkE7QUFBRSwyRkFsQkEseUJBQVUsT0FrQkE7QUFBRSxzRkFsQkEsb0JBQUssT0FrQkE7QUFBRSx5RkFsQkEsdUJBQVEsT0FrQkE7QUFDbkMsMkZBbEJBLHlCQUFVLE9Ba0JBO0FBQUUsdUZBbEJBLHFCQUFNLE9Ba0JBO0FBQUUsdUZBbEJBLHFCQUFNLE9Ba0JBO0FBQUUsK0ZBbEJBLDZCQUFjLE9Ba0JBO0FBQzFDLGlHQWxCQSwrQkFBZ0IsT0FrQkE7QUFoQmpCLCtEQUEwRDtBQVN6RCxvR0FUTyx5Q0FBbUIsT0FTUDtBQVJwQixtQ0FBbUM7QUFTbEMsd0JBQU07QUFSUCx5Q0FBNkM7QUFlNUMsc0ZBZlEsZ0JBQUssT0FlUjtBQUFFLHlGQWZRLG1CQUFRLE9BZVI7QUFkaEIsdUNBQW9DO0FBZW5DLHdGQWZRLGlCQUFPLE9BZVI7QUFiUixNQUFNLE1BQU0sR0FBRyxNQUFNLENBQUMsTUFBTSxDQUFDO0FBTTVCLHdCQUFNIiwic291cmNlc0NvbnRlbnQiOlsiLyogLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiAqIENvcHlyaWdodCAoYykgTWljcm9zb2Z0IENvcnBvcmF0aW9uLiBBbGwgcmlnaHRzIHJlc2VydmVkLlxuICogTGljZW5zZWQgdW5kZXIgdGhlIE1JVCBMaWNlbnNlLiBTZWUgTGljZW5zZS50eHQgaW4gdGhlIHByb2plY3Qgcm9vdCBmb3IgbGljZW5zZSBpbmZvcm1hdGlvbi5cbiAqIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLSAqL1xuJ3VzZSBzdHJpY3QnO1xuXG5pbXBvcnQge1xuXHREZWJ1Z1Nlc3Npb24sXG5cdEluaXRpYWxpemVkRXZlbnQsIFRlcm1pbmF0ZWRFdmVudCwgU3RvcHBlZEV2ZW50LCBDb250aW51ZWRFdmVudCwgT3V0cHV0RXZlbnQsIFRocmVhZEV2ZW50LCBCcmVha3BvaW50RXZlbnQsIE1vZHVsZUV2ZW50LFxuXHRcdExvYWRlZFNvdXJjZUV2ZW50LCBDYXBhYmlsaXRpZXNFdmVudCwgUHJvZ3Jlc3NTdGFydEV2ZW50LCBQcm9ncmVzc1VwZGF0ZUV2ZW50LCBQcm9ncmVzc0VuZEV2ZW50LCBJbnZhbGlkYXRlZEV2ZW50LFxuXHRUaHJlYWQsIFN0YWNrRnJhbWUsIFNjb3BlLCBWYXJpYWJsZSxcblx0QnJlYWtwb2ludCwgU291cmNlLCBNb2R1bGUsIENvbXBsZXRpb25JdGVtLFxuXHRFcnJvckRlc3RpbmF0aW9uXG59IGZyb20gJy4vZGVidWdTZXNzaW9uJztcbmltcG9ydCB7TG9nZ2luZ0RlYnVnU2Vzc2lvbn0gZnJvbSAnLi9sb2dnaW5nRGVidWdTZXNzaW9uJztcbmltcG9ydCAqIGFzIExvZ2dlciBmcm9tICcuL2xvZ2dlcic7XG5pbXBvcnQgeyBFdmVudCwgUmVzcG9uc2UgfSBmcm9tICcuL21lc3NhZ2VzJztcbmltcG9ydCB7IEhhbmRsZXMgfSBmcm9tICcuL2hhbmRsZXMnO1xuXG5jb25zdCBsb2dnZXIgPSBMb2dnZXIubG9nZ2VyO1xuXG5leHBvcnQge1xuXHREZWJ1Z1Nlc3Npb24sXG5cdExvZ2dpbmdEZWJ1Z1Nlc3Npb24sXG5cdExvZ2dlcixcblx0bG9nZ2VyLFxuXHRJbml0aWFsaXplZEV2ZW50LCBUZXJtaW5hdGVkRXZlbnQsIFN0b3BwZWRFdmVudCwgQ29udGludWVkRXZlbnQsIE91dHB1dEV2ZW50LCBUaHJlYWRFdmVudCwgQnJlYWtwb2ludEV2ZW50LCBNb2R1bGVFdmVudCxcblx0XHRMb2FkZWRTb3VyY2VFdmVudCwgQ2FwYWJpbGl0aWVzRXZlbnQsIFByb2dyZXNzU3RhcnRFdmVudCwgUHJvZ3Jlc3NVcGRhdGVFdmVudCwgUHJvZ3Jlc3NFbmRFdmVudCwgSW52YWxpZGF0ZWRFdmVudCxcblx0VGhyZWFkLCBTdGFja0ZyYW1lLCBTY29wZSwgVmFyaWFibGUsXG5cdEJyZWFrcG9pbnQsIFNvdXJjZSwgTW9kdWxlLCBDb21wbGV0aW9uSXRlbSxcblx0RXJyb3JEZXN0aW5hdGlvbixcblx0RXZlbnQsIFJlc3BvbnNlLFxuXHRIYW5kbGVzXG59XG4iXX0=

/***/ })

/******/ });
//# sourceMappingURL=debugAdapter2.js.map