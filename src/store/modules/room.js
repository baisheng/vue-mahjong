/**
 * Created by LISHI on 2017/4/25.
 */
import * as types from '../mutation-types'


const state = {
  showroom: {
    show: false,
    data: {}
  },
  showroomtime: {
    show: false,
    data: {},
    time: ''
  },

  userChose: {
    show: false,
    id: '',
    data: {}
  },
  showEmoji: {
    show: false,
    id: '',
    data: {}
  },
  paiju_record: {},
  rank_record: [],
  my_record: {},
  room_first: {},
  room_record_id: {},
  room_status2: {},
  one_record: {
    // info:{
    //   endTime:1498550445,
    //   id:100000193,
    //   ownerId: 100000296,
    //   roomId: 3,
    //   startTime: 1498548645,
    //   totalTime: 1800,
    //   type: 1
    // },
    // members:[
    //   {
    //     icon:"https://www.baidu.com/img/bd_logo1.png",
    //     name:"ddd123",
    //     playerId:100000299,
    //     score:0,
    //     winFlag:true
    //   },
    //   {
    //     icon:"https://www.baidu.com/img/bd_logo1.png",
    //     name:"ddd123",
    //     playerId:100000299,
    //     score:0,
    //     winFlag:true
    //   },
    //   {
    //     icon:"https://www.baidu.com/img/bd_logo1.png",
    //     name:"ddd123",
    //     playerId:100000299,
    //     score:0,
    //     winFlag:true
    //   },
    //   {
    //     icon:"https://www.baidu.com/img/bd_logo1.png",
    //     name:"ddd123",
    //     playerId:100000299,
    //     score:0,
    //     winFlag:true
    //   },
    //
    // ]
  },
  over: {
    show: false
  }


}
const mutations = {

  [types.SHOW_ENTER_ROOM_ID](state, data) {
    state.showroom = data
  },
  [types.SHOW_ENTER_ROOM_TIME](state, data) {
    state.showroomtime = Object.assign({},state.showroomtime, data)
  },
  [types.FETCH_CLICK_USER](state, data) {
    state.userChose = Object.assign({},state.userChose, data)
  },
  [types.SHOW_EMOJI](state, data) {
    state.showEmoji = data
  },
  [types.FETCH_RANK_RECORD](state, data) {
    state.rank_record = data
  },
  [types.FETCH_MY_RECORD](state, data) {
    state.my_record = data
  },
  [types.FETCH_PAIJU_RECORD](state, data) {
    state.paiju_record = data
  },
  [types.FETCH_ROOM_FISTR](state, data) {
    state.room_first = Object.assign({},state.room_first, data)
  },
  [types.FETCH_ROOM_RECORD_ID](state, data) {
    state.room_record_id = Object.assign({},state.room_record_id, data)
  },
  [types.FETCH_ROOM_STATUS2](state, data) {
    state.room_status2 = Object.assign({},state.room_status2, data)
  },
  [types.FETCH_ROOM_RECORD2](state, data) {
    state.one_record = data
  },
  [types.FETCH_OVER](state, data) {
    state.over = data
  },


}
const getters = {
  showroom: state => state.showroom,
  showroomtime: state => state.showroomtime,
  userChose: state => state.userChose,
  showEmoji: state => state.showEmoji,
  rank_record: state => state.rank_record,
  my_record: state => state.my_record,
  paiju_record: state => state.paiju_record,
  room_first: state => state.room_first,
  room_record_id: state => state.room_record_id,
  room_status2: state => state.room_status2,
  one_record: state => state.one_record,
  over: state => state.over,


}
const actions = {

  initRoomState({commit}, parmas){
    commit(types.FETCH_PAIJU_RECORD, {})
    commit(types.FETCH_ROOM_FISTR, {})
    commit(types.FETCH_ROOM_STATUS2, {})
    commit(types.FETCH_ROOM_FISTR, {})
  },
  //显示进入房间的框
  showEnterRoom({commit}, parmas) {
    commit(types.SHOW_ENTER_ROOM_ID, parmas)
  },
  //显示进入房间选择时间的框
  showEnterRoomTime({commit}, parmas) {
    commit(types.SHOW_ENTER_ROOM_TIME, parmas)
  },
  //获取点击数据
  showUserModal({commit}, parmas) {
    commit(types.FETCH_CLICK_USER, parmas)
  },
  //获取点击数据
  showEmoji({commit}, parmas) {
    commit(types.SHOW_EMOJI, parmas)
  },
  //获取牌局记录
  fetchPaijuRecord({commit}, parmas) {
    commit(types.FETCH_PAIJU_RECORD, parmas)
  },
  //获取战绩记录
  fetchRankRecord({commit}, parmas) {
    commit(types.FETCH_RANK_RECORD, parmas)
  },
  //获取牌局战绩
  fetchMyRecord({commit}, parmas) {
    commit(types.FETCH_MY_RECORD, parmas)
  },
  //获取首次进入房间
  fetchRoomFirst({commit}, parmas) {
    commit(types.FETCH_ROOM_FISTR, parmas)
  },
  //获取记录id
  fetchRoomRecordId({commit}, params){
    commit(types.FETCH_ROOM_RECORD_ID, params)
  },
  //获取房间状态
  fetchRoomStatus({commit}, params){
    commit(types.FETCH_ROOM_STATUS2, params)
  },
  //one_record
  fetchRoomRecord2({commit}, params){
    commit(types.FETCH_ROOM_RECORD2, params)
  },
  fetchOver({commit}, params){
    commit(types.FETCH_OVER, params)
  }

}


export default {
  state,
  mutations,
  getters,
  actions
}
