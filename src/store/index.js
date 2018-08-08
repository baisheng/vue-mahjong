
/**
 * Created by LISHI on 2017/1/31.
 */

import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

import game from './modules/game';
import user from './modules/user';
import room from './modules/room';


// const debug = process.env.NODE_ENV !== 'production'


export  default new Vuex.Store({
  modules: {
    game,
    user,
    room
  },
  strict: false

})
