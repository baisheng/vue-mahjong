<template>

  <div>
    <div class="pay_tip" v-if="over.show">
      <div class="text">
        游戏时间到，本局结束，请到战绩中了解本局情况！
      </div>
      <a href="javascript:;" @click="goHome">返回大厅</a>
      <a href="javascript:;" class="sure" @click="goRank">查看战绩</a>
    </div>
  </div>
</template>


<script>
  import {mapGetters} from 'vuex'
  export default {
    data() {
      return {}
    },
    mounted() {

    },
    created() {

    },
    methods: {

      goHome(){
        this.$store.dispatch('fetchAllUser', {})
        this.$store.dispatch('fetchOver', {show: false})
        this.$router.push({path: '/room2'})
      },

      goRank(){
        this.$store.dispatch('fetchAllUser', {})
        this.$store.dispatch('fetchOver', {show: false})
        this.$router.push({path: `/rankd/${this.room_record_id.recordId}`})
      }

    },

    computed: {
      ...mapGetters({
        'room_first': 'room_first',
        'room_record_id': 'room_record_id',
        'over': 'over',
      })
    }

  }
</script>

<style scoped lang="scss">
  .pay_tip {
    position: fixed;
    top: 50%;
    left: 50%;
    width: 5rem;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    background: #012118;
    padding: .2rem;
    border-radius: 5px;
    z-index: 10000000000;

    .text {
      text-indent: 2em;
      font-size: .26rem;
      line-height: 1.6;
      text-align: left;
      margin-bottom: .3rem;
    }

    a {
      background: #fff;
      color: #219555;
      border-radius: 5px;

      font-size: .24rem;
      padding: .1rem .1rem;
      text-decoration: none;
      display: inline-block;
      width: 2rem;
      margin: .08rem;
      &.sure {
        background: #219555;
        color: #fff;
      }
    }
  }
</style>
